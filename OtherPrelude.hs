{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, GADTs, TypeSynonymInstances #-}
module OtherPrelude where
import Prelude( Show(..), Bool(..), Integer(..), Rational(..), Num(..), (+), (-), (*), (/), (<), (==), (>), (<=), (>=), not, (&&), error, ($), (.) )

-- Склеить два списка за O(length a)
(++) :: [a] -> [a] -> [a]
a ++ b = case a of
		[] 		-> b
		(c:cs)	-> c:(cs ++ b)

-- Список без первого элемента
tail :: [a] -> [a]
tail a = case a of
		[] 		-> error "empty list elem"
		(b:bs) 	-> bs

-- Список без последнего элемента
init :: [a] -> [a]
init a = case a of
		[] 		-> []
		(b:[])	-> []
		(b:bs) 	-> b:(init bs)

-- Первый элемент
head :: [a] -> a
head a = case a of
			[]		-> error "empty"
			(b:bs) 	-> b

-- Последний элемент
last :: [a] -> a
last a = case a of
			[] 	-> error "empty"
			(b:[]) -> b
			(b:bs) -> last bs

-- n первых элементов списка
take :: Integer -> [a] -> [a]
take n a = case n of
		0 -> []
		k -> case a of
			   [] 		-> [] --error "no elem"
			   (x:xs) 	-> x:(take (k - 1) xs)

-- Список без n первых элементов
drop :: Integer -> [a] -> [a]
drop n a = case n of
		0 -> a
		k -> case a of
			[] 		-> []
			(x:xs) 	-> drop (k - 1) xs 


-- Копировать из списка в результат до первого нарушения предиката
-- takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p a = case a of
		[] 		-> []
		(x:xs) 	-> if (p x) == True then x:(takeWhile p xs)
				   else []	


-- Не копировать из списка в результат до первого нарушения предиката,
-- после чего скопировать все элементы, включая первый нарушивший
-- dropWhile (< 3) [1,2,3,4,1,2,3,4] == [3,4,1,2,3,4]
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p a = case a of
			[] 		-> []
			(x:xs) 	-> if (p x) == True then dropWhile p xs
					   else a

-- Разбить список в пару (найбольший префикс удовлетворяющий p, всё остальное)
span :: (a -> Bool) -> [a] -> ([a], [a])
span p a = (takeWhile p a, dropWhile p a)


-- Разбить список по предикату на (takeWhile p xs, dropWhile p xs),
-- но эффективнее
break :: (a -> Bool) -> [a] -> ([a], [a])
break p a = case a of
			 [] 		-> ([], [])
			 (x:xs) 	-> 	if (p x) == True then (x:f, s) 
					    	else ([], a)
					    	where (f, s) = break p xs

-- n-ый элемент списка (считая с нуля)
(!!) :: [a] -> Integer -> a
[] !! n = error "!!: empty list"
l  !! n = 	if (n == 0) then head l
			else if (n < 0) then error "!!: negative n"
			else (tail l) !! (n - 1)


-- Список задом на перёд
reverse :: [a] -> [a]
reverse a = tmprev a []
tmprev [] b = b
tmprev (a:al) b = tmprev al (a:b)


-- (*) Все подсписки данного списка
length :: [a] -> Integer
length [] = 0
length (x:xs) = 1 + length xs 

subseq' :: [a] -> Integer -> [[a]]
subseq' a 1 = [[head a]]
subseq' a k = (take k a):subseq' a (k - 1)

subseq a = reverse $ subseq' a (length a) -- reverse для красоты
subsequences :: [a] -> [[a]]
subsequences [] = []
subsequences a = (subseq a) ++ (subsequences (tail a)) 

-- (*) Все перестановки элементов данного списка
permutations :: [a] -> [[a]]
permutations (x:[]) = (x:[]):[]
permutations a = genmut (head a) xss where xss = permutations (tail a)
genmut :: a -> [[a]] -> [[a]]
genmut x [] = []
genmut x xss = (genlocalmut x (head xss) (length (head xss))) ++ (genmut x (tail xss))
genlocalmut :: a -> [a] -> Integer -> [[a]]
genlocalmut x xs 0 = (x:xs):[]
genlocalmut x xs k = (((take k xs) ++ (x:[])) ++ (drop k xs)):[] ++ (genlocalmut x xs (k - 1))

-- Повторяет элемент бесконечное число раз
repeat :: a -> [a]
repeat a = a:(repeat a)


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
foldl :: (a -> b -> a) -> a -> [b] -> a
foldl f z [] = z
foldl f z (x:xs) = foldl f (f z x) xs


-- Тот же foldl, но в списке оказываются все промежуточные результаты
-- last (scanl f z xs) == foldl f z xs
scanl :: (a -> b -> a) -> a -> [b] -> [a]
scanl f z [] = z:[]
scanl f z (x:xs) = z:(scanl f (f z x) xs)

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
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z [] = z
foldr f z (x:xs) = f x (foldr f z xs) 


-- Аналогично
--  head (scanr f z xs) == foldr f z xs.
scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr f z [] = z:[]
scanr f z (x:xs) = (f x (head tmp)):tmp where tmp = scanr f z xs 

finiteTimeTest = take 10 $ foldr (:) [] $ repeat 1

-- map f l = из первой лабораторной
map f [] = []
map f (x:xs) = (f x):(map f xs)

-- Склеивает список списков в список
concat :: [[a]] -> [a]
concat [] = []
concat (x:xs) = x ++ (concat xs)

-- Эквивалент (concat . map), но эффективнее
concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f [] = []
concatMap f (x:xs) = (f x) ++ (concatMap f xs)

-- Сплющить два списка в список пар длинны min (length a, length b)
zip :: [a] -> [b] -> [(a, b)]
zip a [] = []
zip [] b = []
zip (x:xs) (y:ys) = (x, y):(zip xs ys)

-- Аналогично, но плющить при помощи функции, а не конструктором (,)
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f [] b = []
zipWith f a [] = []
zipWith f (x:xs) (y:ys) = (f x y):(zipWith f xs ys) 

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
		mzero = 0
		mappend = (+)

instance Monoid MulRational where
		mzero = RMult 1
		(RMult a) `mappend` (RMult b) = RMult $ a * b

instance Monoid MulInteger where
		mzero = Mult 1
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
mtfold :: Monoid a => MTree a -> a
mtfold MLeaf = mzero -- А то, что a - моноид нам будет даровано самой природой
mtfold (MNode a l r) = a `mappend` (mtfold l) `mappend` (mtfold r)


-- Напишите терм с типом
-- (...) => MTree a -> x
-- где x -- тип на ваш вкус, (...) - какие-то констреинты (возможно пустые),
-- при этом этот терм внутри должен использовать то, что a -- моноид, но в
-- констреинтах Monoid a быть не должно.
-- Для широты фантазии в терме можно использовать классы типов, определённые в любом
-- месте этого файла.

mterm :: MTree a -> a
mterm MLeaf = mzero
mterm (MNode x _ _) = x

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
instance Group Integer where
	ginv a = mzero - a

instance Group Rational where
	ginv a = mzero - a

instance Group MulRational where
	ginv (RMult a) = RMult(mzero / a)

-- Группу и Абелеву группу в Хаскеле тоже не различить :(
class Group a => Ring a where
    -- mappend из моноида это сложение
    rmul :: a -> a -> a -- а это умножение

-- Определите
instance Ring Integer where
	rmul a b = a * b 

instance Ring Rational where
	rmul a b = a * b 

-- На самом деле коммутативное кольцо, но что поделать
class Ring a => Field a where
    rinv :: a -> a

-- Определите
instance Field Rational where
	rinv a = 1 / a

-- Реализуйте тип для матриц (через списки) и операции над ними
data Matrix a = Ring a => Matrix [[a]]

getTable :: Matrix a -> [[a]]
getTable (Matrix a) = a

instance Show a => Show (Matrix a) where
		show = show . getTable
	
-- Чем должно быть a? Моноидом? Группой? Ещё чем-нибудь?
matsum :: Matrix a -> Matrix a -> Matrix a
matsum (Matrix []) (Matrix []) = Matrix []
matsum (Matrix a) (Matrix []) = Matrix a
matsum (Matrix []) (Matrix b) = Matrix b
matsum (Matrix (x:xs)) (Matrix (y:ys)) = if (length xs == length ys) then Matrix ((stringSum x y):getTable (matsum (Matrix xs) (Matrix ys)))
										 else error "not equals size matrix"
										 
stringSum :: Ring a => [a] -> [a] -> [a]
stringSum s1 s2 = if (length s1 == length s2) then zipWith mappend s1 s2
				  else error "not equals size matrix"

matscalarmul :: Ring a => a -> Matrix a -> Matrix a
matscalarmul _ (Matrix []) = Matrix []
matscalarmul n (Matrix (x:xs)) =  Matrix ((map (rmul n) x):getTable (matscalarmul n (Matrix xs)))

matmul :: Matrix a -> Matrix a -> Matrix a
matmul (Matrix []) (Matrix []) = Matrix []
matmul (Matrix a) (Matrix []) = Matrix []
matmul (Matrix []) (Matrix b) = Matrix []
matmul (Matrix a) (Matrix b) = Matrix (reverse $ getAllElements a b (length a - 1) (length (head b) - 1)) 

getAllElements [] [] _ _ = []
getAllElements a b (-1) k = []
getAllElements a b n k = (reverse $ getStringElements a b n k):(getAllElements a b (n - 1) k)
getStringElements a b n (-1) = []
getStringElements a b n k = (getelem a b n k):(getStringElements a b n (k - 1))

getelem :: Ring a => [[a]] -> [[a]] -> Integer -> Integer -> a
getelem [] _ _ _ = error "no elem"
getelem x y n k = 	let res1 = (x !! n) in
					let res2 = (getcolumn k y) in
					if (length res1 == length res2) then foldr (mappend) mzero (zipWith rmul res1 res2) 
					else error "can't mult"

getcolumn :: Integer -> [[a]] -> [a]
getcolumn k [] = []
getcolumn k (x:xs) = (x !! k):(getcolumn k xs)  
{-
-- (**) Реализуйте классы типов для векторных и скалярных полей.
-- Перепишите в этих терминах что-нибудь из написанного выше.
-- Реализуйте оператор дифференцирования, взятия градиента.
class ? ScalarField ? where
    ?

class ? VectorField ? where
    ?
-}