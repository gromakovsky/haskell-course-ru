{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Tree where

-- Всё что угодно, главное, чтобы соответствовало
-- заданию

-- Надоели велосипеды
import Prelude
import Data.List

data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Show, Read)

-- Добавить элемент, если его нет в дереве
treeInsert :: Ord a => a -> Tree a -> Tree a
treeInsert val Leaf = Node val Leaf Leaf
treeInsert val (Node x l r) = case (val `compare` x) of
                                LT -> Node x (treeInsert val l) r
                                EQ -> Node x l r
                                GT -> Node x l (treeInsert val r)

-- Построить дерево из списка
listToTree :: Ord a => [a] -> Tree a
listToTree = foldr treeInsert Leaf

-- Применить функцию к элементам дерева
treeMap :: (a -> b) -> Tree a -> Tree b
treeMap _ Leaf = Leaf
treeMap f (Node x l r) = Node (f x) (treeMap f l) (treeMap f r)

-- Проверить, есть ли элемент в дереве
treeElem :: Ord a => a -> Tree a -> Bool
treeElem _ Leaf = False
treeElem val (Node x l r) = case (val `compare` x) of
                                LT -> treeElem val l
                                EQ -> True
                                GT -> treeElem val r

-- Удалить элемент, если он есть в дереве
treeErase :: Ord a => a -> Tree a -> Tree a
treeErase _ Leaf = Leaf
-- Сложно :(
treeErase val (Node x l r) = undefined

