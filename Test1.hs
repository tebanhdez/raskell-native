module Test1 where



callMethod1 :: IO (TVar [Song])
callMethod1 param1 = newTVarIO []

type Test1API = 
	"test1" :> Get [testData] -- get of testData

test1Server :: Server Test1API
test1Server = method1 -- :<|> method2
	where
		method1 param1 = callMethod1 param1

