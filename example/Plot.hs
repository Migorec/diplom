module Plot where

import Graphics.EasyPlot
import Models.Theoretical
import Models.Imitational
import Models.Param
import Debug.Trace


axisTitle :: Param -> String
axisTitle K = "Количество пользователей"
axisTitle M = "Количество процессоров"
axisTitle N = "Количество каналов"
axisTitle L = "Количество ремонтных бригад"
axisTitle Lambda = "Интенсивность обдумывания задачи пользователем"
axisTitle Mu = "Интенсивность процесоров"
axisTitle Nu = "Интенсивность каналов"
axisTitle Alpha = "Интенсивность отказов процессоров"
axisTitle Beta = "Интенсивность восстановления процессоров"
axisTitle Gamma = "Интенсивность отказов каналов"
axisTitle Delta = "Интенсивность восстановления каналов"


doPlot :: String -> EParam -> IO ()
doPlot path (EParam b p i f h) = traceShow (EParam b p i f h) $
    do let r = [i, i+h .. f]
           p1 = map (\x -> solve $ set p b x) r
       p2 <- mapM (\x -> performance $ set p b x) r
       writeFile "temp.txt" $ unlines $ map (\(x, y, z) -> show x ++ "\t" ++ 
                                                           show y ++ "\t" ++ 
                                                           show z
                                            ) $ zip3 r p1 p2
       plot' [Debug] (PNG "test.png") $ Gnuplot2D [] [] ("x; set terminal png size 600,500;\n set output \"" ++ path ++ "\";\n set grid;\n set key outside top center;\n set xlabel \"" ++ (axisTitle p) ++"\";\n set ylabel \"Производительность системы\";\n plot \"temp.txt\" using 1:2 with lines lc -1 lw 3 lt 7 ti \"Аналитическая модель\", \"temp.txt\" using 1:3 w l lc -1 lw 3 lt 0 ti \"Имитационная модель\"; set terminal pdf;\n set output \"plot.pdf\";\n set grid;\n set key outside top center;\n set xlabel \"" ++ (axisTitle p) ++"\";\n set ylabel \"Производительность системы\";\n plot \"temp.txt\" using 1:2 with lines lc -1 lw 3 lt 7 ti \"Аналитическая модель\", \"temp.txt\" using 1:3 w l lc -1 lw 7 lt 0 ti \"Имитационная модель\";")
       return ()


