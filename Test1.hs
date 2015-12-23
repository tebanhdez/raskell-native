module Test1 where


data TestData = TestData
	{
		testText :: Text
	}
	deriving (Generic, Show)
instance ToJSON TestData

callMethod1 :: IO (TVar [TestData])
callMethod1 param1 = newTVarIO []

type Test1API = 
	"test1" :> Get [TestData] -- get of testData

test1Server :: Server Test1API
test1Server = method1 -- :<|> method2
	where
		method1 param1 = callMethod1 param1

