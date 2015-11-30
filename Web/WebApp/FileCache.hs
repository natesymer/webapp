{-# LANGUAGE TupleSections #-}

module System.WebApp.FileCache
(
  FileCache(..),
  Entry,
  newFileCache,
  teardownFileCache,
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

-- FileTransform
-- Takes the raw file contents and applies a transform to it
-- The function either returns an error message or the transformed bytestring
type FileTransform = (ByteString -> Either String ByteString)

-- Entry
-- either
--   error message
-- or
--   fst -> error (String) or (gzipped file contents, uncompressed md5)
--   snd -> transform
type Entry = Either String (ByteString, ByteString)

-- FileCache
-- You shouldn't mess with the internals of this data type
data FileCache = FileCache {
  fileCacheBasePath  :: FilePath, -- absolute path on which entry paths are based
  fileCacheHashTable :: MVar (HashTable FilePath (Entry, FileTransform)), -- hash table
  fileCacheFSMonitor :: WatchManager -- watches the directory
}

-- @newFileCache@
-- create a new file cache
-- path -> the base path of the FileCache
--         (i.e.) the path watched by the FileCache. Paths
--                outside this path will not be automatically updated
newFileCache :: FilePath -> IO FileCache
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

-- @teardownFileCache@
-- free resources associated with a FileCache
-- FileCache -> the cache
-- TODO: remove entries from hashtable????
teardownFileCache :: FileCache -> IO ()
teardownFileCache = stopManager . fileCacheFSMonitor

-- @lookup@
-- lookup an entry in the cache
-- fc -> the cache
-- k -> the key used; Should be a path relative to the FileCache's base path
lookup :: FileCache -> FilePath -> IO (Maybe Entry)
lookup (FileCache _ mht _) k = withMVar mht $ \ht -> fmap fst <$> HT.lookup ht k

-- @register@
-- register a FilePath & ReadAction with the cache
-- fc -> the cache
-- timeout -> For values of 0 or less, the entry will not timeout
--            For values of 1 or more, how long the entry will live in the cache
-- k -> the key used; Should be a path relative to the FileCache's base path
-- f -> transform to applied to file contents
register :: FileCache -> Int -> FilePath -> FileTransform -> IO Entry
register fc@(FileCache bp mht _) timeout k f = do
  v <- xformedReadFileE f (bp </> k)
  withMVar mht $ \ht -> HT.insert ht k (v, f)
  when (timeout > 0) $ void $ forkIO $ a
  return v
  where
    a = do
      threadDelay timeout
      invalidate fc k
      
-- see @register@, minus @timeout@
register' :: FileCache -> FilePath -> FileTransform -> IO Entry
register' fc k f = register fc 0 k f

-- see @register@, minus @timeout@ & @readAction@
register'' :: FileCache -> FilePath -> IO Entry
register'' fc k = register fc 0 k Right

-- @refresh@
-- recalculate an entry in the cache
-- fc -> the cache
-- k -> the key used; Should be a path relative to the FileCache's base path
refresh :: FileCache -> FilePath -> IO ()
refresh (FileCache bp mht _) k = withMVar mht f
  where
    f ht = HT.lookup ht k >>= g ht . fmap snd
    g ht (Just xform) = xformedReadFileE xform (bp </> k) >>= HT.insert ht k . (,xform)
    g _  Nothing = return ()

-- @invalidate@
-- invalidate an entry in the cache
-- fc -> the cache
-- k -> the key used; Should be a path relative to the FileCache's base path
invalidate :: FileCache -> FilePath -> IO ()
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