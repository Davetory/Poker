module PokerTester where

    import Poker (deal)
    import Data.List (sort, intersect)
     
    perms = [ [12, 24, 30, 5, 43, 47, 37, 7, 9], [16, 19, 24, 35, 41, 22, 15, 20, 27 ],
            ]
         
    sols  = [   [ "11H",  "11D"],
            ]

    testPerm n = do
        let input = perms !! n
        let shouldBe = sols !! n
        let youSaid = Poker.deal input
        let common = Data.List.intersect shouldBe youSaid
        if (length youSaid) > 5 
            then 0 
            else (fromIntegral $ length common) / (fromIntegral $ length shouldBe)
                
    testPerms 0 = []
    testPerms n = (testPerm $ n-1):testPerms(n-1)
    
    runTests = do
        let scores = testPerms (length perms)
        let pct = 100*(sum scores) / (fromIntegral $ length scores)
        let pct2 = (fromIntegral $ round (pct*10)) / 10.0
        let nPts = (fromIntegral $ round ((sum scores)*10)) / 10.0
        putStrLn $ show nPts ++ "/" ++ show (fromIntegral $ length scores) ++ " marks achieved"
        putStrLn $ show pct2 ++ "%"
        



    