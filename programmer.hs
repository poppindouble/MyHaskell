data OperatingSystem =   GnuPlusLinux
                       | OpenBSDPlusNevermindJustBSDStill
                       | Mac
                       | Windows
                       deriving (Eq, Show)

data ProgrammingLanguage =   Haskell
                           | Agda
                           | Idris
                           | PureScript
                           deriving (Eq, Show)

data Programmer = Programmer { os :: OperatingSystem
                             , lang :: ProgrammingLanguage
                             }
                             deriving (Eq, Show)

allOperatingSystem :: [OperatingSystem]
allOperatingSystem = 
  [ GnuPlusLinux
  , OpenBSDPlusNevermindJustBSDStill
  , Mac
  , Windows
  ]

allLanguages :: [ProgrammingLanguage]
allLanguages =
  [ Haskell
  , Agda
  , Idris
  , PureScript
  ]

allProgrammers :: [Programmer]
allProgrammers = [Programmer {os = o, lang = l} | o <- allOperatingSystem, l <- allLanguages]

passed = length allProgrammers == length allOperatingSystem * length allLanguages