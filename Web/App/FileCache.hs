{-# LANGUAGE TupleSections #-}

{-|
Module      : Web.App.FileCache
Copyright   : (c) Nathaniel Symer, 2015
License     : MIT
Maintainer  : nate@symer.io
Stability   : experimental
Portability : POSIX

Memory-map style file cache. Watches a directory and updates the cache as changes
to files occur. Stores GZIP compressed representations of files and MD5 sums of the
uncompressed representations of the cached files.
-}

module Web.App.FileCache
(
  -- * Types
  -- * Data Structures
  FileCache(..),
  -- * Type Aliases
  Entry,
  FileTransform,
  -- * Creation & Destruction
  newFileCache,
  teardownFileCache,
  -- * Cache Operations
  lookup,
  register,
  register',
  register'',
  refresh,
  invalidate
)
where

import           System.FSNotify
import           System.Directory
import           System.FilePath

import qualified Data.HashTable.IO as HT
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Base16 as B16

import           Control.Monad
import           Control.Concurrent
import           Control.Exception (try, displayException, IOException)

import qualified Codec.Compression.GZip as GZIP (compress)

import qualified Crypto.Hash.MD5 as MD5 (hash)

import           Prelude hiding (lookup)

type HashTable k v = HT.CuckooHashTable k v

-- |Takes the raw file contents and returns either an error (String)
-- or the transformed files contents
type FileTransform = (ByteString -> Either String ByteString)

-- |Either an error message (Left) or a tuple (Right)
-- containing the GZIP'd file contents (fst) and the
-- MD5 sum of the uncompressed file contents (snd)
type Entry = Either String (ByteString, ByteString)

-- |A data structure representing a file cache. Most people
-- shouldn't need to modify this structure.
data FileCache = FileCache {
  fileCacheBasePath  :: FilePath, -- ^ 'FileCache''s base path. All 'FilePath' keys in 'fileCacheHashTable' are relative to this path.
  fileCacheHashTable :: MVar (HashTable FilePath (Entry, FileTransform)), -- ^ See 'fileCachePath'
  fileCacheFSMonitor :: WatchManager -- ^ FSMonitor structure that watches 'fileCacheBasePath'
}

-- |Creates a new 'FileCache'
newFileCache :: FilePath -- ^ Base path of the 'FileCache' to be created
             -> IO FileCache -- ^ The resulting 'FileCache'
newFileCache path@('/':_) = do -- create a file cache based on an absolute path
  fc <- FileCache (addTrailingPathSeparator path) <$> (HT.new >>= newMVar) <*> startManager
  startWatching fc
  return fc
  where
    startWatching fc@(FileCache bp _ m) = void $ watchTree m bp (const True) (f fc)
    f _                     (Added _ _)      = return ()
    f fc@(FileCache bp _ _) (Modified pth _) = refresh fc (makeRelative bp pth)
    f fc@(FileCache bp _ _) (Removed pth _)  = invalidate fc (makeRelative bp pth)
newFileCache path = mkAbsPath path >>= newFileCache -- resolve a relative path to an absolute path
  where mkAbsPath p = (</>) <$> getCurrentDirectory <*> pure p

-- |Frees resources associated with a 'FileCache'
teardownFileCache :: FileCache -> IO ()
teardownFileCache = stopManager . fileCacheFSMonitor

-- |Lookup an entry in the cache
lookup :: FileCache -- ^ 'FileCache' to be used
       -> FilePath -- ^ 'FilePath' to look up
       -> IO (Maybe Entry) -- ^ 'Entry' for the provided 'FilePath'
lookup (FileCache _ mht _) k = withMVar mht $ \ht -> fmap fst <$> HT.lookup ht k

-- |Register a 'FilePath' and 'FileTransform' with the cache
register :: FileCache -- ^ 'FileCache' to register in
         -> Int -- ^ 'Entry' timeout. A timeout of 0 indicates the entry should never expire
         -> FilePath -- ^ 'FilePath' to register
         -> FileTransform -- ^ 'FileTransform' for the 'Entry' to be generated
         -> IO Entry -- ^ Entry for 'FilePath'
register fc@(FileCache bp mht _) timeout k f = do
  v <- xformedReadFileE f (bp </> k)
  withMVar mht $ \ht -> HT.insert ht k (v, f)
  when (timeout > 0) $ void $ forkIO $ a
  return v
  where
    a = do
      threadDelay timeout
      invalidate fc k
      
-- |'register' with a zero timeout
register' :: FileCache -> FilePath -> FileTransform -> IO Entry
register' fc k f = register fc 0 k f

-- |'register' with a zero timeout and no @FileTransform@
register'' :: FileCache -> FilePath -> IO Entry
register'' fc k = register fc 0 k Right

-- |Force reload an entry in the cache
refresh :: FileCache -- ^ 'FileCache' to be used
        -> FilePath -- ^ 'FilePath' to reload
        -> IO ()
refresh (FileCache bp mht _) k = withMVar mht f
  where
    f ht = HT.lookup ht k >>= g ht . fmap snd
    g ht (Just xform) = xformedReadFileE xform (bp </> k) >>= HT.insert ht k . (,xform)
    g _  Nothing = return ()

-- @invalidate@
-- invalidate an entry in the cache
-- fc -> the cache
-- k -> the key used; Should be a path relative to the FileCache's base path
-- |Invalidate an entry in a 'FileCache'
invalidate :: FileCache -- ^ 'FileCache' to be used
           -> FilePath -- ^ 'FilePath' to invalidate
           -> IO ()
invalidate (FileCache _ mht _) k = withMVar mht (flip HT.delete k)

{- Internal -}

readFileE :: FilePath -> IO (Either String ByteString)
readFileE fp = (either (Left . displayException) Right) <$> readF
  where
    readF :: IO (Either IOException ByteString)
    readF = try $ B.readFile fp

-- returns (contents, md5)
xformedReadFileE :: FileTransform -> FilePath -> IO Entry
xformedReadFileE f fp = readFileE fp >>= return . either Left (buildEntry . f)
  where
    buildEntry = either Left (\cnts -> Right (compress cnts, md5 cnts))
    compress = BL.toStrict . GZIP.compress . BL.fromStrict
    md5 = B16.encode . MD5.hash