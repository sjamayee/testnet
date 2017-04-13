-module(active_oracles).
-export([new/0, write/2,get/2,update/1]).

%These are the oracles that allow betting right now.
%This data is available to the VM.

-record(active, {id = 0, %this is the id that the active oracle is stored under. oracles never reuse the same id.
		 question}).%this is the hash of the text of the question that was asked of the oracle. The full question was written on the block that started the oracle.
