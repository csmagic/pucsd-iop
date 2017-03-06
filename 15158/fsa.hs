data Message = Hello | Mail | Rcpt | Rset | Data | Quit | Terminal deriving Eq
data State = InitialState | HelloState | MailState | RcptState | DataState | EndState deriving (Show, Eq)

delta :: State -> Message -> State
delta InitialState Hello = HelloState
delta HelloState Mail = MailState
delta MailState Rcpt = RcptState
delta MailState Rset = HelloState
delta RcptState Data = DataState
delta RcptState Rset = HelloState
delta DataState Terminal = HelloState
delta DataState Quit = DataState
delta _ Quit = EndState
delta state _ = state

finalStates :: [State]
finalStates = [EndState]

isSuccess :: [Message] -> Bool
isSuccess = flip elem finalStates . eval

eval :: [Message] -> State
eval ms = foldl delta InitialState ms
