{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Circuit.Adornt.Simulator (
	-- * Circuit Simulator
	CircuitSimulator,
	prepareSimulator, prepareSimulatorRandom, prepareSimulatorRandomIO,
	step,

	-- * Set Bits of Input Wire
	IWire, setBits, setMultiBits,

	-- * Peek Bits of Output Wire
	OWire, peekOWire, peekMultiOWires,

	-- * Convert between Bits and Word64
	Bits, bitsToWord, wordToBits,
	) where

import Data.Word
import System.Random

import CircuitCore

prepareSimulator :: [Bits] -> CircuitBuilder a -> (a, CircuitSimulator)
prepareSimulator = makeCircuit

setMultiBits :: [IWire] -> [Word64] -> CircuitSimulator -> CircuitSimulator
setMultiBits is vs = foldr (.) id $ zipWith setBits is (wordToBits <$> vs)

peekMultiOWires :: [OWire] -> CircuitSimulator -> [Word64]
peekMultiOWires os cct = bitsToWord . (`peekOWire` cct) <$> os

prepareSimulatorRandom :: RandomGen g => g -> CircuitBuilder a -> (a, CircuitSimulator)
prepareSimulatorRandom g = prepareSimulator (wordToBits <$> randoms g)

prepareSimulatorRandomIO :: CircuitBuilder a -> IO (a, CircuitSimulator)
prepareSimulatorRandomIO cb = (`prepareSimulatorRandom` cb) <$> newStdGen
