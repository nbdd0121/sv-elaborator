# sv-elaborator
A SystemVerilog elaborator

SystemVerilog has many powerful features that really ease hardware development and make code more readable. Some EDA tools have fantastic SystemVerilog support, e.g. QuestaSim, but some support them really poorly and/or buggy, e.g. Quartus.

This project aims to provide an open-source SystemVerilog elaboration tool that can reduces complex SystemVerilog source codes into simple ones. The project is still in its infancy.

Goals:
* Provide a good quality SystemVerilog parser.
* Able to reduce code to simpler form, eventually reduce to Verilog

Non-goals:
* Support all of SystemVerilog. This project mostly will only support synthesisable subset, plus a few useful constructs for simulation.
* Support deprecated SystemVerilog constructs, e.g. operator binding or defparam.
