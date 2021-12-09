data Company = Company {name :: String,
                        employees :: Int,
                        ownerof :: String}
                         deriving (Show)  
                         --Company "Ford" 43 "Mustang,asas,asas,asas" 
--showExpr :: Company -> String


data Entity = Entity    {point :: Double,
                        circle :: Int,
                        container :: String}
                         deriving (Show)  
                         --Entity  42 5  "26,23,27"




data Article = Text String
              | Section String [Article] deriving (Show)

myArticle :: Article
myArticle = Section "Document"[
                Section "Intrudiction" [
                    Text "My introduction",
                    Section "Notation" [Text "alpha beta gamma"]],
                Section "Methods" [
                    Section "Functional Programming" [Text "FPR"],
                    Section "Logical Programing" [Text "LPR"]],
                Section "Results" [Text "All is great"]]

allSectionNames :: Article -> [String]
allSectionNames (Text x) = [x]
allSectionNames (Section (x:xs) ys ) = [xs,ys]
