module Interactive.Command where
import Graphics.UI.GLFW
import Data.BitSet
import Data.List(foldl')

data Command = CLeft | CRight | Up | Down | Turbo | Forward | Backward 
                | ZoomOut | ZoomIn
                | Pause
    deriving (Enum,Bounded,Eq,Ord,Show)

type Commands = BitSet Command

boundKey :: Command -> Key
boundKey x = case x of
                  CLeft -> CharKey 'A'
                  CRight -> CharKey 'D'
                  Forward -> CharKey 'W'
                  Backward -> CharKey 'S'
                  Up -> CharKey 'R'
                  Down -> CharKey 'F'
                  ZoomOut -> CharKey '-'
                  ZoomIn -> CharKey '='
                  Turbo -> KeyLeftShift
                  Pause -> KeySpace

getCommands :: IO (BitSet Command)
getCommands = foldl' f (return empty) [minBound..maxBound]
    where
        f cmdsio cmd = do
            cmds <- cmdsio
            b <- keyIsPressed (boundKey cmd)
            return (if b then insert cmd cmds else cmds)

noCommands :: Commands
noCommands = empty

commanded :: Command -> Commands -> Bool
commanded = member
