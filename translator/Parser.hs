{-# OPTIONS_GHC -w #-}
module Main where
import Token

-- parser produced by Happy Version 1.18.10

data HappyAbsSyn t4 t5
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5

action_0 (6) = happyShift action_3
action_0 (7) = happyShift action_4
action_0 (8) = happyShift action_5
action_0 (9) = happyShift action_6
action_0 (10) = happyShift action_7
action_0 (11) = happyShift action_8
action_0 (12) = happyShift action_9
action_0 (4) = happyGoto action_10
action_0 (5) = happyGoto action_11
action_0 _ = happyFail

action_1 (6) = happyShift action_3
action_1 (7) = happyShift action_4
action_1 (8) = happyShift action_5
action_1 (9) = happyShift action_6
action_1 (10) = happyShift action_7
action_1 (11) = happyShift action_8
action_1 (12) = happyShift action_9
action_1 (5) = happyGoto action_2
action_1 _ = happyFail

action_2 _ = happyFail

action_3 (13) = happyShift action_20
action_3 (15) = happyShift action_21
action_3 _ = happyFail

action_4 (14) = happyShift action_19
action_4 _ = happyFail

action_5 (14) = happyShift action_18
action_5 _ = happyFail

action_6 (14) = happyShift action_17
action_6 _ = happyFail

action_7 (13) = happyShift action_15
action_7 (15) = happyShift action_16
action_7 _ = happyFail

action_8 (14) = happyShift action_14
action_8 _ = happyFail

action_9 (15) = happyShift action_13
action_9 _ = happyReduce_13

action_10 (16) = happyAccept
action_10 _ = happyFail

action_11 (6) = happyShift action_3
action_11 (7) = happyShift action_4
action_11 (8) = happyShift action_5
action_11 (9) = happyShift action_6
action_11 (10) = happyShift action_7
action_11 (11) = happyShift action_8
action_11 (12) = happyShift action_9
action_11 (4) = happyGoto action_12
action_11 (5) = happyGoto action_11
action_11 _ = happyReduce_1

action_12 _ = happyReduce_2

action_13 _ = happyReduce_14

action_14 _ = happyReduce_12

action_15 (15) = happyShift action_25
action_15 _ = happyFail

action_16 (13) = happyShift action_24
action_16 _ = happyReduce_9

action_17 _ = happyReduce_8

action_18 _ = happyReduce_7

action_19 _ = happyReduce_6

action_20 (15) = happyShift action_23
action_20 _ = happyFail

action_21 (13) = happyShift action_22
action_21 _ = happyReduce_3

action_22 (15) = happyShift action_27
action_22 _ = happyFail

action_23 _ = happyReduce_5

action_24 (15) = happyShift action_26
action_24 _ = happyFail

action_25 _ = happyReduce_11

action_26 _ = happyReduce_10

action_27 _ = happyReduce_4

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 ("\t" ++ happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_2  4 happyReduction_2
happyReduction_2 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 ("\t" ++ happy_var_1 ++ "\n" ++ happy_var_2
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_2  5 happyReduction_3
happyReduction_3 (HappyTerminal (Num happy_var_2))
	_
	 =  HappyAbsSyn5
		 ("generate" ++ show happy_var_2
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happyReduce 4 5 happyReduction_4
happyReduction_4 ((HappyTerminal (Num happy_var_4)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Num happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 ("generate ("++show happy_var_2 ++ "," ++ show happy_var_4 ++")"
	) `HappyStk` happyRest

happyReduce_5 = happySpecReduce_3  5 happyReduction_5
happyReduction_5 (HappyTerminal (Num happy_var_3))
	_
	_
	 =  HappyAbsSyn5
		 ("generate (()," ++ show happy_var_3 ++ ")"
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_2  5 happyReduction_6
happyReduction_6 (HappyTerminal (Label happy_var_2))
	_
	 =  HappyAbsSyn5
		 ("queue " ++ show happy_var_2
	)
happyReduction_6 _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_2  5 happyReduction_7
happyReduction_7 (HappyTerminal (Label happy_var_2))
	_
	 =  HappyAbsSyn5
		 ("seize " ++ show happy_var_2
	)
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_2  5 happyReduction_8
happyReduction_8 (HappyTerminal (Label happy_var_2))
	_
	 =  HappyAbsSyn5
		 ("depart " ++ show happy_var_2
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_2  5 happyReduction_9
happyReduction_9 (HappyTerminal (Num happy_var_2))
	_
	 =  HappyAbsSyn5
		 ("advance " ++ show happy_var_2
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happyReduce 4 5 happyReduction_10
happyReduction_10 ((HappyTerminal (Num happy_var_4)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Num happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 ("advance ("++show happy_var_2 ++ "," ++ show happy_var_4 ++ ")"
	) `HappyStk` happyRest

happyReduce_11 = happySpecReduce_3  5 happyReduction_11
happyReduction_11 (HappyTerminal (Num happy_var_3))
	_
	_
	 =  HappyAbsSyn5
		 ("advance (()," ++ (show happy_var_3) ++ ")"
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_2  5 happyReduction_12
happyReduction_12 (HappyTerminal (Label happy_var_2))
	_
	 =  HappyAbsSyn5
		 ("release "++ show happy_var_2
	)
happyReduction_12 _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  5 happyReduction_13
happyReduction_13 _
	 =  HappyAbsSyn5
		 ("terminate "
	)

happyReduce_14 = happySpecReduce_2  5 happyReduction_14
happyReduction_14 (HappyTerminal (Num happy_var_2))
	_
	 =  HappyAbsSyn5
		 ("terminate "++(show happy_var_2)
	)
happyReduction_14 _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 16 16 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	Generate -> cont 6;
	Queue -> cont 7;
	Seize -> cont 8;
	Depart -> cont 9;
	Advance -> cont 10;
	Release -> cont 11;
	Terminate -> cont 12;
	Comma -> cont 13;
	Label happy_dollar_dollar -> cont 14;
	Num happy_dollar_dollar -> cont 15;
	_ -> happyError' (tk:tks)
	}

happyError_ 16 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> HappyIdentity a
happyError' = HappyIdentity . parseError

parse tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError _ = error "Parse error"

translate s = putStrLn ("model = do\n" ++ (parse $ alexScanTokens s))

main = do s <- getContents
          translate s
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 30 "templates/GenericTemplate.hs" #-}








{-# LINE 51 "templates/GenericTemplate.hs" #-}

{-# LINE 61 "templates/GenericTemplate.hs" #-}

{-# LINE 70 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	 (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 148 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
	 sts1@(((st1@(HappyState (action))):(_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 246 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--	trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 312 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
