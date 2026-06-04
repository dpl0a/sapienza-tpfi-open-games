# TPFI Compositional Game Theory
Slides e codebase per la lezione finale del corso di Tecniche di Programmazione Funzionale e Imperativa su [Compositional Game Theory](https://en.wikipedia.org/wiki/Compositional_game_theory).

## Prerequisiti e configurazione dell'ambiente
Questa repo utilizza un flake [Nix](https://nixos.org) per esporre una devshell riproducibile con una versione di GHC con le dipendenze necessarie incluse, oltre alla versione giusta dell'`haskell-language-server` (testata su Mac OS e Linux). Il codice funziona sicuramente anche con la vostra installazione globale di Haskell ma YMMV.

Per installare Nix, si consiglia di utilizzare il [Determinate Nix Installer](https://github.com/DeterminateSystems/nix-installer).

Una volta installato Nix, per attivare la devshell basta utilizzare il comando `nix develop`. Al primo avvio della devshell ci vorrà qualche minuto per scaricare le dipendenze.

Si consiglia inoltre di installare e utilizzare [direnv](https://direnv.net/) e abilitare la gestione automatica dell'ambiente di sviluppo con `direnv allow`. Se usate `vscode` o `emacs`, l'estensione nativa di `direnv` per il vostro editor vi consentirà di utilizzare il language server, se usate `vim`/`nvim`, `helix` o qualsiasi altro editor da terminale con supporto per LSP, vi basterà aprire l'editor dopo aver attivato la devshell.

## Risorse aggiuntive
### Game Theory Base
- Yoav Shoham, Kevin Leyton-Brown - Multi-Agent Systems [[link]](https://www.masfoundations.org/mas.pdf)

### Compositional Game Theory
- Neil Ghani, Jules Hedges, Viktor Winschel, Philipp Zahn - Compositional game theory [[arXiv]](https://arxiv.org/abs/1603.04641)
- Joe Bolt, Jules Hedges, Philipp Zahn - Bayesian open games [[arXiv]](https://arxiv.org/abs/1910.03656v2) **ATTENZIONE: Questo paper e il precedente usano una rappresentazione diversa di Open Game rispetto a quella moderna. La rappresentazione dell'arena vista a lezione deriva da:**
- Matteo Capucci, Neil Ghani, Jérémy Ledent, Fredrik Nordvall Forsberg - Translating Extensive Form Games to Open Games with Agency [[arXiv]](https://arxiv.org/abs/2105.06763)
- Matteo Capucci - Diegetic Representation of Feedback in Open Games [[arXiv]](https://arxiv.org/abs/2206.12338) **ATTENZIONE: Questo paper ha una presentazione sbagliata della reverse derivative di lenti parametriche vista a lezione, è interessante per capire il contesto e il ragionamento ma potrebbe confondervi.**

### Altre applicazioni di Optics
- Matteo Capucci, Bruno Gavranović, Jules Hedges, Eigil Fjeldgren Rischel - Towards Foundations of Categorical Cybernetics [[arXiv]](https://arxiv.org/abs/2105.06332)
- Bruno Gavranovic - Fundamental Components of Deep Learning: A Category-Theoretic Approach [[link]](https://github.com/bgavran/bgavran.github.io/blob/master/assets/FundamentalComponentsOfDeepLearning.pdf)

- Potete trovare una vecchia versione open source dell'Open Games Engine [quì](https://github.com/CyberCat-Institute/open-game-engine).


## Altro
- Per qualsiasi domanda: `nomecognome \[at\] protonmail.com`
- [Institute for Categorical Cybernetics](https://cybercat.institute)
- [Un vecchio blog post dove spiego parte di quello che abbiamo visto a lezione](https://cybercat.institute/2024/04/22/open-games-bootcamp-i/)
