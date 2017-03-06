{-1) First we need to understand the nature of the State type constructor.
     Below is an example where we have a student record in Haskell. Try loading
     it in the interpreter. We automatically get the following accessor 
     functions for free
         a) name       :: Student -> String
         b) rollno     :: Student -> Int
         c) year       :: Student -> Int
         d) degree     :: Student -> String -}

data Student = Student { name    :: String,
     	       	       	 rollno  :: Int,
			 year	 :: Int,	
			 degree  :: String}

data Student1 = StudentD String Int Int String

stuVal = Student "Preety" 2000 2 "MCA" 

-- The following Syntax is useful if all fields are not in order
stuVal1 = Student {name="Preety", year=2, degree= "MCA", rollno=2000}

{- Try the 2 lines below to see how it works in the interpreter

--> :t stuVal
--> :t stuVal1
--> name StuVal
     	     
-}

data FuncType x = Lambda(x -> (x,x)) 
data FuncType1 x = Lambda1{func :: x -> (x,x)} 

{-2 Recall the State Monad definition. Remember that newType can be used instead  of Data when there is one value constructor with one field

    newType State s a = State { runState :: (s -> (a,s)}

    So now is it easy to understand what is runState and its type?

    :t runState
    --> State s a -> s -> (a,s)

     Makes sense?

     Anyway lets start moving away from the old defn and the first step is
     a new defn for State. Mind you this is still not the current definition

     newType State s a = State (s -> (a,s))

     easy?

     Read this as a function from one state (s) to a desired result (a) and
     a new state whose type again is (s)

     Now we have  a function runState available in our library whose name is 
     runState and type is

     runState :: State s a -> s -> (a,s)

     We also have the following functions available

     evalState :: State s a -> s -> a
     evalState m s = fst (runState m s)

     execState :: State s a -> s -> s
     execState m s = snd (runState m s)
    
     All the above functions take a Value whose type is State s a ,unwrap the
     "State" constructor apply it to an argument and either return the tuple,
     or the first element of the tuple or the second element of the tuple 
     respectively.

     How do we make State and  instance of the monad class?. Recall how Either 
     type became an monad instance. 

     intance Monad (State s) where
          return x = State (\s -> (x,s))
          m >>= f  = State (\s ->
                                   let (x,newstate) = runState m s
                                   in runState (f x) newstate)

     Recall that in the let syntax the final RHS experssion is after the 'in' 
     keyword . i.e. runState (f x) newstate

     or if I might quote the style of 'Learn you ...'
     
      Let m = (State h)

      (State h) >>= f = State (\s -> let (x,newstate) = h s
                                           (State g)    = f x
                                      in g newstate
     


     "State s" is a type constructor just like "Either a" and the kind is 
     * -> *

     The type of bind for the State Monad is

      (>>=) :: State s a -> (a -> State s b) -> State s b

      So what does bind do?

      Well lets recall why we need the state monad. Well if we need to carry
      around the state in any computation we have to add an extra parameter as
      we discussed in the example of the arithmethic expression evaluator.

      In other words the expression-evaluation computation needed an extra 
      argument for counting the number of times the function was 
      called. For non-trivial computations this approach could get ugly as well
      as error-prone.

      Before we go ahead we need to understand the type of (>>) for the State
      monad

      (>>) :: State s a -> State s b -> State s b

      (Remember it is ma -> mb -> mb)

      Now keeping the above definitions of (>>=) and (>>)in mind lets see what 
      other functionalities I need with the state.

      1) I need to retrieve the current state at a given point in the 
         computation
      2) Likewise, I need a way of changing the current state.

      So I need a get and a put n. 

      The Types and Definitions of put and get are
      
      put :: s -> State s ()
      put n = State (\_ -> ((),n)) 

      get :: State s s
      get = State (\s -> (s,s))

      "put n " is a lambda (wrapped inside State) which ignores the argument its
      applied to and gives back a tuple ((), n)
      
      get is a lambda (wrapped inside State) which replaces the result - the 
      first element of the tuple with the current state.

      Eg:

       get >>= \x -> put (x+1)

      Remember that the above is a "STATE lambda" expression. Henceforth we 
      wont  mention the State constructor and the wrapping and unwrapping. I 
      will only speak in terms of lambda and you must keep in mind the wrapping
      and unwrapping happening in the background. 

     So what kind of lambda is it? . The only way I evaluate a lambda is to 
     apply it. 

     (1) runState (get >>= \x -> put (x+1)) 1

     At this stage go up and revisit the definition of runState and (>>=)
     ,put and get

     (2) runState (State (\y -> (y,y)) >>= \x -> State (\_ -> ((),x+1))  1

     Can you now work out the rest including what return does?

       
-- Buffer output

Prelude Control.Monad.State> :t runState
runState :: State s a -> s -> (a, s)
Prelude Control.Monad.State> return "x" :: State Int String

<interactive>:6:1:
    No instance for (Show (State Int String))
      arising from a use of `print'
    Possible fix:
      add an instance declaration for (Show (State Int String))
    In a stmt of an interactive GHCi command: print it

Prelude Control.Monad.State> :t runState
runState :: State s a -> s -> (a, s)

Prelude Control.Monad.State> runState (return "x" :: State Int String) 0
Loading package transformers-0.4.1.0 ... linking ... done.
("x",0)

Prelude Control.Monad.State> evalState (return "x" :: State Int String) 0
"x"

Prelude Control.Monad.State> :t runState get 1
runState get 1 :: Num s => (s, s)

Prelude Control.Monad.State> runState get 1
Loading package mtl-2.2.1 ... linking ... done.
(1,1)

Prelude Control.Monad.State> runState (put 5) 1
((),5)

Prelude Control.Monad.State> runState (put 5 >> get) 1
(5,5)

Prelude Control.Monad.State> runState (get >>= put ) 1
((),1)

Prelude Control.Monad.State> runState (get >>= \x -> put (x+1) ) 1
((),2)

Prelude Control.Monad.State> runState (gets (+1)) 1
(2,1)

Prelude Control.Monad.State> :t evalState
evalState :: State s a -> s -> a

Prelude Control.Monad.State> : execState
execState :: State s a -> s -> s      

-}
                 
