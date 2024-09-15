Config { font = "Bitstream Vera Sans Mono 8"
       , additionalFonts =
          [ "FontAwesome Bold 8"
          , "FontAwesome 14"
          , "Hack 19"
          , "Hack 14"
          ]
       , border = NoBorder
       , bgColor = "#2B2E37"
       , fgColor = "#929AAD"
       , alpha = 255
       , position = TopSize L 100 10
       , lowerOnStart = True
       , allDesktops = False
       , persistent = False
       , hideOnStart = False
       , iconRoot = ".config/xmonad/xmobar/icons/"
       , commands =
         [ Run XMonadLog
         , Run Date "%a, %d %b  <fn=5>󰥔</fn>   %H:%M:%S" "date" 10
         , Run Memory ["-t","Mem: <fc=#AAC0F0><usedratio></fc>%"] 10
         , Run Com "uname" ["-s", "-r"] "" 0
         , Run Weather "RJTT" [ "--template", "<skyCondition> | <fc=#4682B4><tempC></fc>°C"
                              ] 36000
         , Run DiskU [ ("/",      "/ <used> (<size>)")
                     , ("/store", "/store <used> (<size>)")
                     ] [] 60
         , Run Volume "default" "Master" [] 10
        -- network activity monitor (dynamic interface resolution)
         , Run DynNetwork     [ "--template" , "<dev>: <tx>kB/s|<rx>kB/s"
                              , "--Low"      , "1000"       -- units: B/s
                              , "--High"     , "5000"       -- units: B/s
                              , "--low"      , "darkgreen"
                              , "--normal"   , "darkorange"
                              , "--high"     , "darkred"
                              ] 10

         -- cpu activity monitor
         , Run MultiCpu       [ "--template" , "Cpu: <total0> | <total1> | <total2> | <total3> | <total4> | <total5> | <total6> | <total7> | <total8> | <total9> | <total10> | <total11> | <total12> | <total13> | <total14> | <total15>"
                              , "--Low"      , "50"         -- units: %
                              , "--High"     , "85"         -- units: %
                              , "--low"      , "darkgreen"
                              , "--normal"   , "darkorange"
                              , "--high"     , "darkred"
                              ] 10
         ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%multicpu% || %XMonadLog% |}  %date%  %RJTT% {| %default:Master% | %memory% | %dynnetwork% || %disku%"
       }
