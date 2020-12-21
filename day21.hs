{-# LANGUAGE TupleSections #-}
import Utils
import Data.Ord
import qualified Data.Set as S
import Data.List

type Ingredient = String
type Allergen = String

allergens :: Parser [(Allergen,S.Set Ingredient)]
allergens = do
  ingredients <- many1 letter `endBy` spaces
  string "(contains "
  allergens <- many1 letter `sepBy` string ", "
  char ')'
  let ingredientSet = S.fromList ingredients
  return $ map (,ingredientSet) allergens

findImpossibleIngredientsForAllergen :: S.Set Ingredient -> Allergen -> [(Allergen,S.Set Ingredient)] -> S.Set Ingredient
findImpossibleIngredientsForAllergen allIngredients a ins = S.difference allIngredients maybes
  where maybes = foldl' S.intersection allIngredients ingredientSetsForAllergen
        ingredientSetsForAllergen = map snd $ filter (\x -> fst x == a) ins

ordNub = S.toList . S.fromList

part1 = do
  Right parseResult <- parseFileLines allergens "day21_input.txt"
  let allAllergens = ordNub . map fst $ concat parseResult
  let allIngredients = foldl' S.union S.empty $ map snd $ concat parseResult
  let impossibleIngredientsForAllAllergens = map (\a -> findImpossibleIngredientsForAllergen allIngredients a (concat parseResult)) allAllergens
  let cantBeAllergen = foldl' S.intersection allIngredients impossibleIngredientsForAllAllergens
  -- parseResults has some lists multiple times, one each for all allergens in the product, so we need to dedupe that
  let allIngredientLists = map (snd . head) parseResult
  print . sum $ map (\a -> sum [ 1 | i <- allIngredientLists, S.member a i]) $ S.toList cantBeAllergen

findPossibleIngredientsForAllergen :: S.Set Ingredient -> S.Set Ingredient -> Allergen -> [(Allergen,S.Set Ingredient)] -> (Allergen,S.Set Ingredient)
findPossibleIngredientsForAllergen knownInert allIngredients a ins = (a,S.difference maybes knownInert)
  where maybes = foldl' S.intersection allIngredients ingredientSetsForAllergen
        ingredientSetsForAllergen = map snd $ filter (\x -> fst x == a) ins

inferSolutions :: [(Allergen,S.Set Ingredient)] -> [(Allergen,Ingredient)] -> String
inferSolutions [] foundSoFar = intercalate "," $ map snd $ sortBy (comparing fst) foundSoFar
inferSolutions as foundSoFar = inferSolutions (map (\(a,s) -> (a,S.delete nextIngredient s)) $ filter (\(a,_) -> a /= nextAllergen) as) ((nextAllergen, nextIngredient) : foundSoFar)
  where (nextAllergen, nextIngredient) = (\(a,s) -> (a, head $ S.toList s)) . head $ filter (\(a,s) -> S.size s == 1) as

part2 = do
  Right parseResult <- parseFileLines allergens "day21_input.txt"
  let allAllergens = ordNub . map fst $ concat parseResult
  let allIngredients = foldl' S.union S.empty $ map snd $ concat parseResult
  let impossibleIngredientsForAllAllergens = map (\a -> findImpossibleIngredientsForAllergen allIngredients a (concat parseResult)) allAllergens
  let cantBeAllergen = foldl' S.intersection allIngredients impossibleIngredientsForAllAllergens
  let candidates = map (\a -> findPossibleIngredientsForAllergen cantBeAllergen allIngredients a (concat parseResult)) allAllergens
  print $ inferSolutions candidates []
