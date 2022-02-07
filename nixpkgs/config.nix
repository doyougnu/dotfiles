{
    allowUnfree = true;
    allowBroken = true;
    doCheck     = false;
    ghc.version = "ghc8107";
    packageOverrides = super: let self = super.pkgs; in
    {

        rEnv = super.rWrapper.override {
            packages = with self.rPackages; [
                devtools
                ggplot2
                reshape2
                yaml
                optparse
                tidyr
                cowplot
                ggthemes
                svglite
                latex2exp
                forcats
                broom
                Hmisc
                ggpubr
                rstatix
                QuantPsyc
                bit64
                gdata
                tidyverse
                knitr
                lemon # for pretty printing
                ];
        };
    };
}
