{
module Token where
}

%wrapper "basic"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters


tokens :-
    $white+				;
    $digit+				{ \s -> Num (read s) }
    GENERATE			{ \s -> Generate }
    QUEUE			{ \s -> Queue }
    SEIZE			{ \s -> Seize }
    DEPART			{ \s -> Depart }
    ADVANCE			{ \s -> Advance }
    RELEASE			{ \s -> Release}
    TERMINATE			{ \s -> Terminate }
    $alpha+          {\s -> Label s}
    ","           {\s -> Comma}
  
  
{

data Token = Generate | Queue | Seize | Depart | Advance | Release | Terminate |
             Comma | Num Double | Label String | Default deriving (Eq, Show)

}
