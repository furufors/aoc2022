#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
import Data.List
import qualified Data.Map as M
import Data.Char
import Debug.Trace
type File = (String,Int)
data Elem = Folder (String, Int, [Elem]) | File File deriving Show

main :: IO ()
main = interact $ show . calFolders . calculate . toTree (Folder ("root", 0, [])) . addPaths [] . lines
--  . calFolders . calculate . toTree (Folder ("root", 0, []))
--  toTree (Folder ("/",[]))
-- sum . filter (<100000) . map (snd)
--main = interact $ show . addPaths [] . lines

calFolders :: Elem -> Int
calFolders (File _) = 0
calFolders (Folder (_,i,es)) = if i < 100000 then i + sum (map calFolders es) else sum (map calFolders es)

calculate :: Elem -> Elem
calculate (File (s,i)) = File (s,i)
calculate (Folder (n,_,es)) = Folder (n, sum (map cal es), map calculate es)

cal :: Elem -> Int
cal (File (s,i)) = i
cal (Folder (n,_,es)) = sum (map cal es)

toTree :: Elem -> [([String],String)] -> Elem
toTree t [] = t
toTree t ((folders,file):ss) = toTree (treeIns t folders file) ss

treeIns :: Elem -> [String] -> String -> Elem
treeIns (Folder (s, i, es))  [] n =
    let size = read $ takeWhile isDigit $ n
    in Folder (s, i, File (n, size):es)
treeIns (Folder (s, i, es)) (f:fs) n =
    if f `elem` subfolders es
    then Folder (s, 0, [if fn == f then treeIns (Folder (fn,0,fes)) fs n else Folder (fn,0,fes) | (Folder (fn,0,fes)) <- es] ++ [File f | (File f) <- es])
    else Folder (s, 0, es ++ [treeIns (Folder (f,0,[])) fs n])

subfolders :: [Elem] -> [String]
subfolders = map folderName . filter isFolder

folderName :: Elem -> String
folderName (Folder (s,_,_)) = s
folderName (File _) = error "File has no folderName"

isFolder :: Elem -> Bool
isFolder (Folder _) = True
isFolder (File   _) = False

addPaths :: [String] -> [String] -> [([String],String)]
addPaths _ [] = []
addPaths prefix (s:ss) | isPrefixOf "$ ls" s = addPaths prefix ss
addPaths prefix (s:ss) | isPrefixOf "dir" s = addPaths prefix ss
addPaths prefix (s:ss) | isPrefixOf "$ cd .." s = addPaths (init prefix) ss
addPaths prefix (s:ss) | isPrefixOf "$ cd " s = addPaths (prefix ++ [drop 5 s]) ss
addPaths prefix (s:ss) = (prefix,s):addPaths prefix ss