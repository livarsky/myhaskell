{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, GADTs #-}
module OtherPrelude where
import Prelude

-- Склеить два списка за O(length a)
(+++) :: [a] -> [a] -> [a]
a +++ b = case a of
		[] 		-> b
		(c:cs)	-> (c:(cs +++ b))


-- Список без первого элемента
tail1 :: [a] -> [a]
tail1 a = case a of
		[] 		-> error "empty list elem"
		(b:bs) 	-> bs

-- Список без последнего элемента
init1 :: [a] -> [a]
init1 a = case a of
		[] 		-> []
		(b:[])	-> []
		(b:bs) 	-> b:(init1 bs)

-- Первый элемент
head1 :: [a] -> a
head1 a = case a of
			[]		-> error "emp1ty"
			(b:bs) 	-> b

-- Последний элемент
last1 :: [a] -> a
last1 a = case a of
			[] 	-> error "empty"
			(b:[]) -> b
			(b:bs) -> last1 bs

-- n первых элементов списка
take1 :: Integer -> [a] -> [a]
take1 n a = case n of
		0 -> []
		k -> case a of
			   [] 		-> [] --error "no elem"
			   (x:xs) 	-> x:(take1 (k - 1) xs)

-- Список без n первых элементов
drop1 :: Integer -> [a] -> [a]
drop1 n a = case n of
		0 -> a
		k -> case a of
			[] 		-> []
			(x:xs) 	-> drop1 (k - 1) xs 


-- Копировать из списка в результат до первого нарушения предиката
-- takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
takeWhile1 :: (a -> Bool) -> [a] -> [a]
takeWhile1 p a = case a of
		[] 		-> []
		(x:xs) 	-> if (p x) == True then x:(takeWhile1 p xs)
				   else []	


-- Не копировать из списка в результат до первого нарушения предиката,
-- после чего скопировать все элементы, включая первый нарушивший
-- dropWhile (< 3) [1,2,3,4,1,2,3,4] == [3,4,1,2,3,4]
dropWhile1 :: (a -> Bool) -> [a] -> [a]
dropWhile1 p a = case a of
			[] 		-> []
			(x:xs) 	-> if (p x) == True then dropWhile1 p xs
					   else a

-- Разбить список в пару (найбольший префикс удовлетворяющий p, всё остальное)
span1 :: (a -> Bool) -> [a] -> ([a], [a])
span1 p a = (takeWhile1 p a, dropWhile1 p a)


-- Разбить список по предикату на (takeWhile p xs, dropWhile p xs),
-- но эффективнее
break1 :: (a -> Bool) -> [a] -> ([a], [a])
break1 p a = case a of
			 [] 		-> ([], [])
			 (x:xs) 	-> 	if (p x) == True then (x:f, s) 
					    	else ([], a)
					    	where (f, s) = break1 p xs

-- n-ый элемент списка (считая с нуля)
(!!!) :: [a] -> Integer -> a
[] !!! n = error "!!: empty list"
l  !!! n = 	if (n == 0) then head1 l
			else if (n < 0) then error "!!!: negative n"
			else (tail1 l) !!! (n - 1)


-- Список задом на перёд
reverse1 :: [a] -> [a]
reverse1 a = tmprev a []
tmprev [] b = b
tmprev (a:al) b = tmprev al (a:b)


-- (*) Все подсписки данного списка
length1 :: [a] -> Integer
length1 [] = 0
length1 (x:xs) = succ (length1 xs) 

subseq' :: [a] -> Integer -> [[a]]
subseq' a 1 = [[head1 a]]
subseq' a k = (take1 k a):subseq' a (k - 1)

subseq a = reverse1 $ subseq' a (length1 a) -- reverse для красоты
subsequences :: [a] -> [[a]]
subsequences [] = [[]]
subsequences a = (subseq a) ++ (subsequences (tail1 a)) 

-- (*) Все перестановки элементов данного списка
permutations :: [a] -> [[a]]
permutations (x:[]) = (x:[]):[]
permutations a = genmut (head1 a) xss where xss = permutations (tail1 a)
genmut :: a -> [[a]] -> [[a]]
genmut x [] = []
genmut x xss = (genlocalmut x (head1 xss) (length1 (head1 xss))) ++ (genmut x (tail1 xss))
genlocalmut :: a -> [a] -> Integer -> [[a]]
genlocalmut x xs 0 = (x:xs):[]
genlocalmut x xs k = (((take1 k xs) ++ (x:[])) ++ (drop1 k xs)):[] ++ (genlocalmut x xs (k - 1))

-- Повторяет элемент бесконечное число раз
repeat1 :: a -> [a]
repeat1 a = a:(repeat1 a)


-- Левая свёртка
-- порождает такое дерево вычислений:
--         f
--        / \
--       f   ...
--      / \
--     f   l!!2
--    / \
--   f   l!!1
--  / \
-- z  l!!0
foldl2 :: (a -> b -> a) -> a -> [b] -> a
foldl2 f z [] = z
foldl2 f z (x:xs) = foldl2 f (f z x) xs


-- Тот же foldl, но в списке оказываются все промежуточные результаты
-- last (scanl f z xs) == foldl f z xs
scanl2 :: (a -> b -> a) -> a -> [b] -> [a]
scanl2 f z [] = z:[]
scanl2 f z (x:xs) = z:(scanl2 f (f z x) xs)

-- Правая свёртка
-- порождает такое дерево вычислений:
--    f
--   /  \
-- l!!0  f
--     /  \
--   l!!1  f
--       /  \
--    l!!2  ...
--           \
--            z
--            
foldr2 :: (a -> b -> b) -> b -> [a] -> b
foldr2 f z [] = z
foldr2 f z (x:xs) = f x (foldr2 f z xs) 


-- Аналогично
--  head (scanr f z xs) == foldr f z xs.
scanr2 :: (a -> b -> b) -> b -> [a] -> [b]
scanr2 f z [] = z:[]
scanr2 f z (x:xs) = (f x (head1 tmp)):tmp where tmp = scanr2 f z xs 

finiteTimeTest = take 10 $ foldr (:) [] $ repeat 1

-- map f l = из первой лабораторной
map1 f [] = []
map1 f (x:xs) = (f x):(map1 f xs)

-- Склеивает список списков в список
concat1 :: [[a]] -> [a]
concat1 [] = []
concat1 (x:xs) = x ++ (concat1 xs)

-- Эквивалент (concat . map), но эффективнее
concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f [] = []
concatMap f (x:xs) = (f x) ++ (concatMap f xs)

-- Сплющить два списка в список пар длинны min (length a, length b)
zip :: [a] -> [b] -> [(a, b)]
zip a b = ?

{-
-- Аналогично, но плющить при помощи функции, а не конструктором (,)
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith = ?

-- Интересные классы типов
class Monoid a where
    mzero :: a
    mappend :: a -> a -> a

instance Monoid [a] where
    mzero = []
    mappend = (++)

instance Monoid Integer where
    mzero = 0
    mappend = (+)

data MulInteger = Mult Integer
data MulRational = RMult Rational

-- Реализуйте инстансы Monoid для Rational и MulRational
instance Monoid Rational where
    ?

instance Monoid MulRational where
    ?

instange Monoid MulInteger where
    mzero = 1
    (Mult a) `mappend` (Mult b) = Mult $ a * b

-- Фолдабл
class MFoldable t where
    mfold :: Monoid a => t a -> a

-- Альтернативный фолдабл
class Monoid a => AMFoldable t a where
    amfold :: t a -> a
-- Изучите раздницу между этими двумя определениями.

-- Смотрите какой чит. Можно построить дерево только из элементов моноида.
data MTree a = Monoid a => MLeaf | MNode a (MTree a) (MTree a)

-- Выпишите тип этого выражения. Фигурирует ли в нём Monoid? Почему?
mtfold MLeaf = mzero -- А то, что a - моноид нам будет даровано самой природой
mtfold (MNode a l r) = a `mappend` (mtfold l) `mappend` (mtfold r)

-- Напишите терм с типом
-- (...) => MTree a -> x
-- где x -- тип на ваш вкус, (...) - какие-то констреинты (возможно пустые),
-- при этом этот терм внутри должен использовать то, что a -- моноид, но в
-- констреинтах Monoid a быть не должно.
-- Для широты фантазии в терме можно использовать классы типов, определённые в любом
-- месте этого файла.
mterm = ?

-- (**) Разберитесь чем отличаются эти определения.
-- "Скомпилируйте" их в наш гипотетический язык программирования с
-- типом Dict.
instance MFoldable MTree where
    mfold = mtfold

instance Monoid a => AMFoldable MTree a where
    amfold = mtfold

--------- Тут переделаем немного
-- Группа
--class Group a where
--    gzero :: a
--    ginv  :: a -> a
--    gmult :: a -> a -> a
--
--class Group Integer where
--    gzero = 0
--    ginv a = -a
--    gmult = (+)
--
--class Group MulInteger where
--    ? это я погорячился, да

-- Хаскель слабоват для нормального определения всех этих штук.
-- Кольцо вообще непонятно как определить, потому что группы и полугруппы
-- должны быть по паре (тип, операция).
class Monoid a => Group a where
    ginv :: a -> a

-- Определите
--instance Group для Integer, Rational, MulRational

-- Группу и Абелеву группу в Хаскеле тоже не различить :(
class Group a => Ring a where
    -- mappend из моноида это сложение
    rmul :: a -> a -> a -- а это умножение

-- Определите
--instance Ring для Integer, Rational

-- На самом деле коммутативное кольцо, но что поделать
class Ring a => Field a where
    rinv :: a -> a

-- Определите
--instance Field для Rational

-- Реализуйте тип для матриц (через списки) и операции над ними
data Matrix a = ?
-- Чем должно быть a? Моноидом? Группой? Ещё чем-нибудь?

matsum = ?

matscalarmul = ?

matmul = ?

-- (**) Реализуйте классы типов для векторных и скалярных полей.
-- Перепишите в этих терминах что-нибудь из написанного выше.
-- Реализуйте оператор дифференцирования, взятия градиента.
class ? ScalarField ? where
    ?

class ? VectorField ? where
    ?
-}