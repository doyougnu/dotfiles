{
    allowUnfree = true;
    allowBroken = true;
    doCheck     = false;
    ghcVersion  = "ghc924";
    packageOverrides = super: let self = super.pkgs; in
    {

        # haskell-env = self.haskell.packages.${ghcVersion}.ghcWithHoogle
        #     (haskellPackages: with haskellPackages; [
        #         arrows async mtl transformers base containers criterion gauge tasty tasty-bench
        #         tasty-golden unagi-chan basement cassava shelly cabal unordered-containers sbv
        #         hasktags hlint
        #     ]);

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

        pyEnv = super.buildEnv {
            name = "myPy";
            paths = [
                # A Python 3 interpreter with some packages
                (self.python3.withPackages (
                    ps: with ps; [
                        pyflakes
                        pytest
                        pandas
                        numpy
                        matplotlib
                    ]
                ))
                # Some other packages we'd like as part of this env
                # self.ripgrep
                # self.tmux
            ];
        };
    };
}
