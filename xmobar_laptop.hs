-- xmobar config used by Vic Fryzel
-- Author: Vic Fryzel
-- http://github.com/vicfryzel/xmonad-config

-- This is setup for dual 1920x1080 monitors, with the right monitor as primary
Config {
    font = "xft:Fixed-8",
    bgColor = "#000000",
    fgColor = "#ffffff",
    position = Static { xpos = 0, ypos = 0, width = 1440, height = 16 },
    lowerOnStart = True,
    commands = [
        Run Weather "KCVO" ["-t","<tempF>F <skyCondition>","-L","64","-H","77","-n","#CEFFAC","-h","#FFB6B0","-l","#96CBFE"] 36000,
        Run MultiCpu ["-t","Cpu: <total0> <total1> <total2> <total3>","-L","30","-H","60","-h","#FFB6B0","-l","#CEFFAC","-n","#FFFFCC","-w","3"] 10,
        Run Memory ["-t","Mem: <usedratio>%","-H","8192","-L","4096","-h","#FFB6B0","-l","#CEFFAC","-n","#FFFFCC"] 10,
        Run Network "wlp3s0" ["-t","Net: <rx>, <tx>","-H","200","-L","10","-h","#FFB6B0","-l","#CEFFAC","-n","#FFFFCC"] 10,
        Run Date "%a %b %_d %k:%M" "date" 10,
        Run StdinReader,
        Run Battery [ "--template", "Batt: <acstatus>", "--Low", "10" -- units: %, "--High", "80", -- units: %, "--low" , "darkred"
                      , "--normal" , "darkorange", "--high" , "darkgreen", "--", "-o", "<left>% (<timeleft>)", "-O"
                      , "<fc=#dAA520>Charging</fc>", "-i", "<fc=#006000>Charged</fc>"
                    ] 10
    ],
    sepChar = "%",
    alignSep = "}{",
    template = "%StdinReader% }{ %multicpu%   %memory%   %battery%   %wlp3s0%   <fc=#FFFFCC>%date%</fc>   %KCVO%"
}
