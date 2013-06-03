module GUI (gui, module WX)where

import Data.IORef
import Graphics.UI.WX
import Graphics.UI.WX  as WX (start)

import Data.Binding.Simple
import Graphics.UI.WX.Binding

import Models.Param hiding (set)
import Plot



gui = do --create widgits
         window <- frame [text := "Data Binding with WxHaskell"]
         
         k <- hslider window False 1 50 [selection := 5]
         kTitle <- staticText window [text := "Количество пользователей: ", clientSize := sz 230 20]
         kVal <- staticText window [text := "5", clientSize := sz 40 20]
         kVariable <- variable [value := 5]
         set k [on command := do v <- get k selection
                                 set kVariable [value := v]
                                 set kVal [text := show v]
               ]
               
         lambda <- hslider window False 1 300 [selection := 10]
         lambdaTitle <- staticText window [text := "Интенсивность обдумывания: ", clientSize := sz 230 20]
         lambdaVal <- staticText window [text := "1.0", clientSize := sz 40 20]
         lambdaVariable <- variable [value := 1.0] :: IO (Var Double)
         set lambda [on command := do v <- get lambda selection
                                      let r = fromIntegral v / 10
                                      set lambdaVariable [value := r]
                                      set lambdaVal [text := show r]
                    ]
         
         m <- hslider window False 1 50 [selection := 5]
         mTitle <- staticText window [text := "Количество процесоров: ", clientSize := sz 230 20]
         mVal <- staticText window [text := "5", clientSize := sz 40 20]
         mVariable <- variable [value := 5]
         set m [on command := do v <- get m selection
                                 set mVariable [value := v]
                                 set mVal [text := show v]
               ]

         mu <- hslider window False 1 300 [selection := 10]
         muTitle <- staticText window [text := "Интенсивность обработки: ", clientSize := sz 230 20]
         muVal <- staticText window [text := "1.0", clientSize := sz 40 20]
         muVariable <- variable [value := 1.0] :: IO (Var Double)
         set mu [on command := do v <- get mu selection
                                  let r = fromIntegral v / 10
                                  set muVariable [value := r]
                                  set muVal [text := show r]
                ]

         n <- hslider window False 1 50 [selection := 5]
         nTitle <- staticText window [text := "Количество каналов: ", clientSize := sz 230 20]
         nVal <- staticText window [text := "5", clientSize := sz 40 20]
         nVariable <- variable [value := 5]
         set n [on command := do v <- get n selection
                                 set nVariable [value := v]
                                 set nVal [text := show v]
               ]

         nu <- hslider window False 1 300 [selection := 10]
         nuTitle <- staticText window [text := "Интенсивность обработки: ", clientSize := sz 230 20]
         nuVal <- staticText window [text := "1.0", clientSize := sz 40 20]
         nuVariable <- variable [value := 1.0] :: IO (Var Double)
         set nu [on command := do v <- get nu selection
                                  let r = fromIntegral v / 10
                                  set nuVariable [value := r]
                                  set nuVal [text := show r]
                ]

         alpha <- hslider window False 0 300 [selection := 10]
         alphaTitle <- staticText window [text := "Интенсивность отказов процессоров: ", clientSize := sz 230 20]
         alphaVal <- staticText window [text := "0.0", clientSize := sz 40 20]
         alphaVariable <- variable [value := 0.0] :: IO (Var Double)
         set alpha [on command := do v <- get alpha selection
                                     let r = fromIntegral v / 10
                                     set alphaVariable [value := r]
                                     set alphaVal [text := show r]
                ]

         beta <- hslider window False 1 300 [selection := 10]
         betaTitle <- staticText window [text := "Интенсивность восстановления процессоров: ", clientSize := sz 230 20]
         betaVal <- staticText window [text := "0.1", clientSize := sz 40 20]
         betaVariable <- variable [value := 0.1] :: IO (Var Double)
         set beta [on command := do v <- get beta selection
                                    let r = fromIntegral v / 10
                                    set betaVariable [value := r]
                                    set betaVal [text := show r]
                ]

         gamma <- hslider window False 0 300 [selection := 10]
         gammaTitle <- staticText window [text := "Интенсивность отказов каналов: ", clientSize := sz 230 20]
         gammaVal <- staticText window [text := "0.0", clientSize := sz 40 20]
         gammaVariable <- variable [value := 0.0] :: IO (Var Double)
         set gamma [on command := do v <- get gamma selection
                                     let r = fromIntegral v / 10
                                     set gammaVariable [value := r]
                                     set gammaVal [text := show r]
                ]

         delta <- hslider window False 1 300 [selection := 10]
         deltaTitle <- staticText window [text := "Интенсивность восстановления каналов: ", clientSize := sz 230 20]
         deltaVal <- staticText window [text := "0.1", clientSize := sz 40 20]
         deltaVariable <- variable [value := 0.1] :: IO (Var Double)
         set delta [on command := do v <- get delta selection
                                     let r = fromIntegral v / 10
                                     set deltaVariable [value := r]
                                     set deltaVal [text := show r]
                ]

         l <- hslider window False 1 50 [selection := 5]
         lTitle <- staticText window [text := "Количество ремонтных бригад: ", clientSize := sz 230 20]
         lVal <- staticText window [text := "5", clientSize := sz 40 20]
         lVariable <- variable [value := 5]
         set l [on command := do v <- get l selection
                                 set lVariable [value := v]
                                 set lVal [text := show v]
               ]

         radio <- radioBox window Vertical ["","","","","","","","","","",""] [clientSize := sz 280 20,
                                                                               border := BorderNone]

         vbitmap <- variable [value := Nothing]

         pic <- scrolledWindow window [scrollRate := sz 600 520,
                                       clientSize := sz 600 520, on paint := onPaint vbitmap,
                                       bgcolor := white, fullRepaintOnResize := False]

         

         run <- button window [text := "Пуск!", on command := do rad <- get radio selection
                                                                 --print test 
                                                                 mp <- do k <- get kVariable value
                                                                          m <- get mVariable value
                                                                          n <- get nVariable value
                                                                          l <- get lVariable value
                                                                          lambda <- get lambdaVariable value
                                                                          mu <- get muVariable value
                                                                          nu <- get nuVariable value
                                                                          alpha <- get alphaVariable value
                                                                          beta <- get betaVariable value
                                                                          gamma <- get gammaVariable value
                                                                          delta <- get deltaVariable value
                                                                          return $ MParam m n k l lambda mu nu alpha beta gamma delta
                                                                 doPlot "test.png" (eParam rad mp)
                                                                 openImage pic vbitmap "test.png" 
                              ]

         set window [layout :=row 0[alignTop $ widget radio,
                                     column 0 [hfill $ vspace 20 ,
                                          row 0 [vfloatCentre $ widget kTitle,
                                                 vfloatCentre $ widget kVal, hfill $ widget k], 
                                          row 0 [vfloatCentre $ widget lambdaTitle,
                                                 vfloatCentre $ widget lambdaVal, hfill $ widget lambda],
                                          row 0 [vfloatCentre $ widget mTitle,
                                                 vfloatCentre $ widget mVal, hfill $ widget m],
                                          row 0 [vfloatCentre $ widget muTitle,
                                                 vfloatCentre $ widget muVal, hfill $ widget mu],
                                          row 0 [vfloatCentre $ widget nTitle,
                                                 vfloatCentre $ widget nVal, hfill $ widget n],
                                          row 0 [vfloatCentre $ widget nuTitle,
                                                 vfloatCentre $ widget nuVal, hfill $ widget nu],
                                          row 0 [vfloatCentre $ widget alphaTitle,
                                                 vfloatCentre $ widget alphaVal, hfill $ widget alpha],
                                          row 0 [vfloatCentre $ widget betaTitle,
                                                 vfloatCentre $ widget betaVal, hfill $ widget beta],
                                          row 0 [vfloatCentre $ widget gammaTitle,
                                                 vfloatCentre $ widget gammaVal, hfill $ widget gamma],
                                          row 0 [vfloatCentre $ widget deltaTitle,
                                                 vfloatCentre $ widget deltaVal, hfill $ widget delta],
                                          row 0 [vfloatCentre $ widget lTitle,
                                                 vfloatCentre $ widget lVal, hfill $ widget l],
                                          hfloatRight $ widget run
                                        ], widget pic],
                     clientSize := sz 1100 520]
                      
    
closeImage vbitmap
      = do mbBitmap <- swap vbitmap value Nothing
           case mbBitmap of
             Nothing -> return ()
             Just bm -> objectDelete bm
                     
openImage sw vbitmap fname
      = do -- load the new bitmap
           bm <- bitmapCreateFromFile fname  -- can fail with exception
           closeImage vbitmap
           set vbitmap [value := Just bm]
           -- reset the scrollbars 
           bmsize <- get bm size
           set sw [virtualSize := bmsize]
           repaint sw
       --`catch` \err -> repaint sw
       
onPaint vbitmap dc viewArea
      = do mbBitmap <- get vbitmap value
           case mbBitmap of
             Nothing -> return () 
             Just bm -> drawBitmap dc bm pointZero False []
             
eParam :: Int -> MParam -> EParam
eParam i mp =  [EParam mp K 1 50 1,EParam mp Lambda 0.1 30 0.1,
                EParam mp M 1 50 1,EParam mp Mu 0.1 30 0.1,
                EParam mp N 1 50 1,EParam mp Nu 0.1 30 0.1,
                EParam mp Alpha 0 30 0.1, EParam mp Beta 0.1 30 0.1,
                EParam mp Gamma 0 30 0.1, EParam mp Delta 0.1 30 0.1,
                EParam mp L 1 30 1] !! i
