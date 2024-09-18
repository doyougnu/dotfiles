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
       , position = BottomSize L 100 10
       , lowerOnStart = True
       , allDesktops = False
       , persistent = False
       , hideOnStart = False
       , iconRoot = ".config/xmonad/xmobar/icons/"
       , commands =
         [
           Run Memory ["-t","Mem: <fc=#AAC0F0><used></fc>/<total>"] 10
         -- cpu activity monitor
         , Run MultiCpu       [ "--template" , "<total0> | <total1> | <total2> | <total3> | <total4> | <total5> | <total6> | <total7> | <total8> | <total9> | <total10> | <total11> | <total12> | <total13> | <total14> | <total15>"
                              , "--Low"      , "50"         -- units: %
                              , "--High"     , "85"         -- units: %
                              , "--low"      , "darkgreen"
                              , "--normal"   , "darkorange"
                              , "--high"     , "darkred"
                              ] 10
         ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "CPU: %multicpu% | }  {|  %memory%MB "
       }
