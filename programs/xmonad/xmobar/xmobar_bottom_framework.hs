Config { font = "Bitstream Vera Sans Mono 12"
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
       , position = BottomSize L 100 25
       , lowerOnStart = True
       , allDesktops = False
       , persistent = False
       , hideOnStart = False
       , iconRoot = ".config/xmonad/xmobar/icons/"
       , commands =
         [ Run XMonadLog
         , Run Memory ["-t","Mem: <fc=#AAC0F0><used></fc>/<total>"] 10
         -- cpu activity monitor
         , Run MultiCpu       [ "--template" , "Cpu: <total0> | <total1> | <total2> | <total3> | <total4> | <total5> | <total6> | <total7> "
                              , "--Low"      , "50"         -- units: %
                              , "--High"     , "85"         -- units: %
                              , "--low"      , "darkgreen"
                              , "--normal"   , "darkorange"
                              , "--high"     , "darkred"
                              ] 10
         ,    Run BatteryP ["BAT1"]
                ["-t", "<acstatus><watts> W (<left>%) / <timeleft>",
                 "-L", "10", "-H", "80", "-p", "3",
                 "--", "-O", "<fc=green>On</fc> - ", "-i", "",
                 "-L", "-15", "-H", "-5",
                 "-l", "red", "-m", "blue", "-h", "green"]
                600
         ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%multicpu% | } %battery%  {|  %memory% "
       }
