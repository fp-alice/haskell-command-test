module Commands where

import           CommandParser
import           Mod

update :: Setting -> Setting -> Setting
update (Float _)  new@(Float _)  = new
update (List _)   new@(List _)   = new
update (Int _)    new@(Int _)    = new
update (Bool _)   new@(Bool _)   = new
update (String _) new@(String _) = new
update old        _              = old

updateModSetting :: Mod -> String -> Setting -> (Setting -> Setting -> Setting) -> Mod
updateModSetting (Mod mn ms me) target value fn =  Mod mn (fmap updateSetting ms) me
  where updateSetting (settingName, settingValue) = if settingName == target
                                                    then (settingName, fn settingValue value)
                                                    else (settingName, settingValue)

addSettings :: Setting -> Setting -> Setting
addSettings (Float current) (Float new) = Float (current + new)
addSettings (Float current) (Int new)   = Float (current + fromIntegral new)
addSettings (Int current)   (Int new)   = Int   (current + new)
addSettings (Int current)   (Float new) = Int   (current + round new)
addSettings (List current)  (List new)  = List  (current ++ new)
addSettings (List current)  new         = List  (current ++ [new])
addSettings current         _           = current

subtractSettings :: Setting -> Setting -> Setting
subtractSettings (Float current) (Float new) = Float (current - new)
subtractSettings (Float current) (Int new)   = Float (current - fromIntegral new)
subtractSettings (Int current)   (Int new)   = Int   (current - new)
subtractSettings (Int current)   (Float new) = Int   (current - round new)
subtractSettings (List current)  (List new)  = List  (filter (`notElem` new) current)
subtractSettings (List current)  new         = List  (filter (/= new) current)
subtractSettings current         _           = current

interpret :: Instruction -> Setting -> String -> Mod -> Mod
interpret Set    v name m              = updateModSetting m name v update
interpret Add    v name m              = updateModSetting m name v addSettings
interpret Remove v name m              = updateModSetting m name v subtractSettings
interpret Toggle _ _    (Mod mn ms me) = Mod mn ms (not me)

{-|
Peculiar bug, if we go into the repl and enter the following
xray blocks + [grass dirt]
xray's block setting contains [String "grass", String "dirt"]
if I then do
xray blocks - dirt
xray's block setting contains []
when I would expect it to contain [String "grass"]

This is peculiar, because doing the same thing in-code produces the expected result

updateModSetting (Mod "xray" [("blocks", List[String "grass", String "stone"])] False) "blocks" (String "grass") subtractSettings
and
interpret Remove (String "grass") "blocks" (Mod "xray" [("blocks", List [String "grass", String "dirt"])] False)
both produce the expected result
-}

runCommand :: Command -> [Mod] -> [Mod]
runCommand (Command instruction targetMod settingName currentValue) = fmap updateMod
  where updateMod mod'@(Mod name _ _) = if name == targetMod
                                        then interpret instruction currentValue settingName mod'
                                        else mod'

run mods i = case parseCommand mods i of
  Left e  -> (show e, commands)
  Right r -> (show r, runCommand r commands)

repl :: IO ()
repl = go commands where
  go mods = do
    mapM_ print mods
    input <- getLine
    let (s, newMods) = run mods input
    putStrLn s
    go newMods
