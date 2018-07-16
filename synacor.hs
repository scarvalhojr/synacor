
import           Data.Char            (chr, ord)
import           Data.List            (intersperse)
import           Data.Word            (Word16)
import           Data.Bits            ((.&.), (.|.), complement)
import           System.IO            (openFile, IOMode(ReadMode))
import           System.Environment   (getArgs)
import qualified Data.ByteString as B (ByteString, hGetContents, unpack)
import qualified Data.Vector     as V (Vector, fromList, toList, replicate,
                                       take, drop, (++), (!), (//))


data Instruction = HALTcode
                 | SETcode Word16 Word16
                 | PUSHcode Word16
                 | POPcode Word16
                 | EQcode Word16 Word16 Word16
                 | GTcode Word16 Word16 Word16
                 | JMPcode Word16
                 | JTcode Word16 Word16
                 | JFcode Word16 Word16
                 | ADDcode Word16 Word16 Word16
                 | MULTcode Word16 Word16 Word16
                 | MODcode Word16 Word16 Word16
                 | ANDcode Word16 Word16 Word16
                 | ORcode Word16 Word16 Word16
                 | NOTcode Word16 Word16
                 | RMEMcode Word16 Word16
                 | WMEMcode Word16 Word16
                 | CALLcode Word16
                 | RETcode
                 | OUTcode Word16
                 | INcode Word16
                 | NOOPcode
                 | UNKNcode Word16
  deriving (Eq, Show)

type Program = [Word16]
type ProgCounter = Word16
type Registers = [Word16]
type Stack = [Word16]
type Memory = V.Vector Word16

data Runtime = Runtime {
  getPC :: ProgCounter,
  getRegisters :: Registers,
  getStack :: Stack,
  getMemory :: Memory
}

numRegs :: Int
numRegs = 8

maxValue :: Word16
maxValue = 32767

maxReg :: Word16
maxReg = maxValue + fromIntegral numRegs

main :: IO ()
main = do
  args <- getArgs
  case args of
    []     -> putStrLn "Usage: synacor <program binary file>"
    (a:as) -> do file <- openFile a ReadMode
                 input <- B.hGetContents file
                 runProg $ parseProg input

runProg :: Program -> IO ()
runProg prog
  | length prog > maxm  = putStrLn "Code does not fit in memory."
  | otherwise           = execProg $ Runtime 0 regs [] mem
  where regs = replicate numRegs 0
        maxm = fromIntegral (maxValue + 1)
        free = max 0 (maxm - length prog)
        mem  = V.fromList prog V.++ V.replicate free (0 :: Word16)

parseProg :: B.ByteString -> Program
parseProg xs = combine (B.unpack xs)
  where combine []         = []
        combine [x]        = [fromIntegral x]
        combine (x1:x2:xs) = combine' x1 x2 : combine xs
        combine' x1 x2     = 256 * (fromIntegral x2) + fromIntegral x1

execProg :: Runtime -> IO ()
execProg rt@(Runtime pc _ _ mem) =
  case fetchInstr pc mem of
    Nothing           -> putStrLn "Program terminated unexpectedly."
    Just (UNKNcode x) -> putStrLn $ "ERROR: unknown or incomplete instruction "
                                    ++ show x ++ "."
    Just HALTcode     -> putStrLn "Program completed successfully."
    Just i            -> execInstr rt i >>= execProg

fetchInstr :: ProgCounter -> Memory -> Maybe Instruction
fetchInstr pc = matchInstr . V.toList . V.drop (fromIntegral pc)

matchInstr :: [Word16] -> Maybe Instruction
matchInstr []            = Nothing
matchInstr (0:xs)        = Just (HALTcode)
matchInstr (1:a:b:xs)    = Just (SETcode a b)
matchInstr (2:a:xs)      = Just (PUSHcode a)
matchInstr (3:a:xs)      = Just (POPcode a)
matchInstr (4:a:b:c:xs)  = Just (EQcode a b c)
matchInstr (5:a:b:c:xs)  = Just (GTcode a b c)
matchInstr (6:a:xs)      = Just (JMPcode a)
matchInstr (7:a:b:xs)    = Just (JTcode a b)
matchInstr (8:a:b:xs)    = Just (JFcode a b)
matchInstr (9:a:b:c:xs)  = Just (ADDcode a b c)
matchInstr (10:a:b:c:xs) = Just (MULTcode a b c)
matchInstr (11:a:b:c:xs) = Just (MODcode a b c)
matchInstr (12:a:b:c:xs) = Just (ANDcode a b c)
matchInstr (13:a:b:c:xs) = Just (ORcode a b c)
matchInstr (14:a:b:xs)   = Just (NOTcode a b)
matchInstr (15:a:b:xs)   = Just (RMEMcode a b)
matchInstr (16:a:b:xs)   = Just (WMEMcode a b)
matchInstr (17:a:xs)     = Just (CALLcode a)
matchInstr (18:xs)       = Just (RETcode)
matchInstr (19:a:xs)     = Just (OUTcode a)
matchInstr (20:a:xs)     = Just (INcode a)
matchInstr (21:xs)       = Just (NOOPcode)
matchInstr (x:xs)        = Just (UNKNcode x)

execInstr :: Runtime -> Instruction -> IO Runtime
execInstr rt@(Runtime pc rs st mem) inst =
  case inst of
    SETcode  a b   -> return $ setReg rt a (eval b) 3
    PUSHcode a     -> return $ pushInstr rt (eval a)
    POPcode  a     -> return $ popInstr rt a
    EQcode   a b c -> return $ compInstr rt a (eval b == eval c)
    GTcode   a b c -> return $ compInstr rt a (eval b > eval c)
    JMPcode  a     -> return $ jmpInstr rt (eval a)
    JTcode   a b   -> return $ condJumpInstr rt (eval a /= 0) (eval b)
    JFcode   a b   -> return $ condJumpInstr rt (eval a == 0) (eval b)
    ADDcode  a b c -> return $ setReg rt a (eval b `addOpr` eval c) 4
    MULTcode a b c -> return $ setReg rt a (eval b `multOpr` eval c) 4
    MODcode  a b c -> return $ setReg rt a (eval b `mod` eval c) 4
    ANDcode  a b c -> return $ setReg rt a (eval b .&. eval c) 4
    ORcode   a b c -> return $ setReg rt a (eval b .|. eval c) 4
    NOTcode  a b   -> return $ setReg rt a (notOpr (eval b)) 3
    RMEMcode a b   -> return $ rmemInstr rt a (eval b)
    WMEMcode a b   -> return $ wmemInstr rt (eval a) (eval b)
    CALLcode a     -> return $ callInstr rt (eval a)
    RETcode        -> return $ retInstr rt
    OUTcode  a     -> outInstr rt (eval a)
    INcode   a     -> inInstr rt a
    NOOPcode       -> return $ Runtime (pc + 1) rs st mem
    _              -> error $ "ERROR: invalid instruction code: " ++ show inst
  where eval x
          | x <= maxValue  = x
          | x <= maxReg    = rs !! fromIntegral (x - maxValue - 1)
          | otherwise      = undefined -- TODO handle invalid values

setReg :: Runtime -> Word16 -> Word16 -> Word16 -> Runtime
setReg (Runtime pc rs st mem) a val jump = Runtime (pc + jump) rs' st mem
  where rs' = updateRegs rs a val

updateRegs :: Registers -> Word16 -> Word16 -> Registers
updateRegs rs r x
  | i >= 0 && i < numRegs  = take i rs ++ x : drop (i + 1) rs
  | otherwise              = undefined -- TODO handle invalid register number
  where i = fromIntegral (r - maxValue - 1)

addOpr :: Word16 -> Word16 -> Word16
addOpr a b = (a + b) `mod` (maxValue + 1)

notOpr :: Word16 -> Word16
notOpr a = maxValue .&. complement a

multOpr :: Word16 -> Word16 -> Word16
multOpr a b = (a * b) `mod` (maxValue + 1)

pushInstr :: Runtime -> Word16 -> Runtime
pushInstr (Runtime pc rs st mem) a = Runtime (pc + 2) rs (a : st) mem

popInstr :: Runtime -> Word16 -> Runtime
popInstr (Runtime pc rs (x:st) mem) a = Runtime (pc + 2) rs' st mem
  where rs' = updateRegs rs a x

compInstr :: Runtime -> Word16 -> Bool -> Runtime
compInstr (Runtime pc rs st mem) a truth = Runtime (pc + 4) rs' st mem
  where rs' = updateRegs rs a (if truth then 1 else 0)

jmpInstr :: Runtime -> Word16 -> Runtime
jmpInstr (Runtime _ rs st mem) a = Runtime a rs st mem

condJumpInstr :: Runtime -> Bool -> Word16 -> Runtime
condJumpInstr (Runtime pc rs st mem) truth b = Runtime pc' rs st mem
  where pc' = if truth then b else pc + 3

rmemInstr :: Runtime -> Word16 -> Word16 -> Runtime
rmemInstr (Runtime pc rs st mem) a b = Runtime (pc + 3) rs' st mem
  where rs' = updateRegs rs a (mem V.! fromIntegral b)

wmemInstr :: Runtime -> Word16 -> Word16 -> Runtime
wmemInstr (Runtime pc rs st mem) a b = Runtime (pc + 3) rs st mem'
  where mem' = mem V.// [(fromIntegral a, b)]

callInstr :: Runtime -> Word16 -> Runtime
callInstr (Runtime pc rs st mem) a = Runtime a rs (pc + 2 : st) mem

retInstr :: Runtime -> Runtime
retInstr (Runtime pc rs (x:st) mem) = Runtime x rs st mem

outInstr :: Runtime -> Word16 -> IO Runtime
outInstr (Runtime pc rs st mem) a = do
  putChar $ chr (fromIntegral a)
  return $ Runtime (pc + 2) rs st mem

inInstr :: Runtime -> Word16 -> IO Runtime
inInstr rt@(Runtime pc rs st mem) a = do
  ch <- getChar
  case ch of
    '$' -> inInstr (secretInjection rt) a
    _   -> do let code = fromIntegral (ord ch)
              return $ Runtime (pc + 2) (updateRegs rs a code) st mem

-- Inject code to bypass teleporter check, stolen from
-- https://github.com/lux01/synacor
secretInjection :: Runtime -> Runtime
secretInjection (Runtime pc rs st mem) = Runtime pc rs st mem'
  where mem' = mem V.// [ (5451, 1), (5452, 32775), (5453, 25734)
                        , (5483, 1), (5484, 32768), (5485, 6), (5486, 21)
                        , (5487, 21), (5488, 21), (5489, 21), (5490, 21)
                        ]
