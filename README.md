# Get Your Freq On

I'm exploring radio frequencies that a device on the side of my
house transmits every so often (I think it is [this one](https://fccid.io/OWS-NIC514)).

I think I figured out the frequency it likes to transmit using [this utility I updated](https://github.com/csm/gr-scan) and landed on 912.6 MHz.

I'm then demodulating FSK using [this GNU Radio spec](https://github.com/csm/getyourfreqon/blob/main/grc/fsk-test2.grc) and turning it into binary data. Doing this yielded some interesting sequences of bytes, so I started this Clojure project to do some more investigation.

It's still early days with this repository.