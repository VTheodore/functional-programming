{--
  СУ "Св. Климент Охридски"
  Факултет по математика и информатика
  Курс Функционално програмиране 2020/21
  Контролно 3
  2021-01-16

  Име: Теодор Везенков
  ФН: 62467
  Специалност: СИ
  Курс: 2
  Административна група: Е
  Начален час на контролното: 08:00
--}

module K3_62467_Test where

import Test.HUnit
import K3_62467

mapsToTests = TestList [

  "a = 1, b = 3 with f x -> x ^ 2 should be (1, 9)" ~:
    (1, 9) ~=? mapsTo (\ x -> x * x) 1 3,

  "a = 1, b = 3 with f x -> -x should be (-3, -1)" ~:
    (-3, -1) ~=? mapsTo (\ x -> (-x)) 1 3

  "a = b = 3 with f x -> x should be (3, 3)" ~:
    (3, 3) ~=? mapsTo id 3 3 
  ]

main = do
  runTestTT mapsToTests