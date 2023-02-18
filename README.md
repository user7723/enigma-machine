# Enigma machine model

### Description

Enigma is a command line program that (relatively inefficiently) encrypts your data either from a file or from `<stdin>`, and writes its output to a specified file or to `<stdout>`. It does not abide by the constraint of 26 symbols from the original hardware implementation, and currently it maps not the characters, but the bytes that represent them, so it's capable of encrypting text constituted with any set of characters.

It's written in Haskell, and uses it's laziness features to build tree of choice for rotors, and reflectors by their "serial numbers".

The rotor is represented as a pair of byte-arrays (one that maps toward the Enigma's reflector, and the other one that goes backwards), each index of byte in the array denotes mapping between the rotor pins, i.e. each index of an array is the input pin of given rotor's side, and the byte available by that index is the output pin on the opposite side of the rotor. The set of all possible rotors is equal to permutation of bytes from `0x00` to `0xff` by `256` elements in each permutation, which is `256!` possible rotors (you can acquire the exact number by `enigma --bounds-info`)

The reflector is represented as an array, similarly to the rotors, but it maps pairs of pins on the same side, thus, the amount of mappings that the reflector represents is the half of it's pins, which is 128. The amount of possible reflectors is counted in the following way:

```
n = 256
m = n / 2
n! / m! * 2^m
```

The [formula](https://www.physicsforums.com/threads/number-of-pairings-in-a-set-of-n-objects.668904/) works in the following way:
  - there is n! ways to permute n objects without repetition
  - m is the amount of pairs we can get out of n objects - n/2.
  - Pairs in that tree of permutations may be arranged in m! ways
  - and for each pair we want to exclude their commutation, so
  - for each of m-pairs we divide by 2, thus we have 2₁* 2₂ * ... * 2ₘ which is 2ᵐ

Triple of rotors is packed in the magazine which holds information of each rotor state (offset). That state information is used in the mapping process.

The process of encryption roughly works in the following way:

1. map input byte - `bᵢ` using pair of offsets

    - `s₀` for the input plate, since it is always stays still, it's equal to `0`
    - `s₁` for the first rotor's state
    - `s₁` - `s₀` + `bᵢ` (it is important to understand that the arithmetic *unsigned* and done by *modulo 256*)
    - result is `b₀₋₁` - **at this point the `input byte` was passed through the `input plate` and through a pin on right hand side of the `first rotor` and went out as a "new byte" (new pin on left hand side, essentially, whose index represents that byte)**

2. map input byte emitted from the `first rotor` - `b₀₋₁` using pair of offsets:

    - `s₁` for the first rotor's state
    - `s₂` for the second rotor's state
    - `s₂` - `s₁` + `b₀₋₁`
    - the result of mapping between the `first` and the `second rotors` is `b₁₋₂` - **at this point the `input byte` was emitted from the left side of the `first rotor` and using pair of offsets it was passed to corresponding pin on a right side of a `second rotor`, from where it was mapped to a pin on it's left side**

3. map input byte emitted from the `second rotor` - `b₁₋₂` using pair of offsets:

    - `s₂` for the second rotor's state
    - `s₃` for the third rotor's state
    - `s₃` - `s₂` + `b₁₋₂`
    - the result of mapping between the second and the third rotor is `b₂₋₃` - **at this point the `input byte` was emitted from the left side of the `second rotor` and using pair of offsets it was passed to corresponding pin on a right side of a `third rotor`, from where it was mapped to a pin on it's left side**

4. map input byte emitted from the `third rotor` - `b₂₋₃` using pair of offsets:

    - `s₃` for the third rotor's state
    - `sᵣ` for the reflector's state, which is equal to `0`
    - `sᵣ` - `s₃` + `b₂₋₃`
    - the result of mapping between the second and the third rotor is `b₃₋ᵣ` - **at this point the `input byte` was emitted from the left side of the `third rotor` and using pair of offsets it was passed to corresponding pin on a right side of the `reflector`, inside the reflector it was mapped back to some oft the third rotor's left side pins, lets assume that it is `bₒ` pin**

The same logic is used to map `bₒ` from left to right, but the states are used in different order: initially it was `(s₀,s₁) (s₁,s₂) (s₂,s₃) (s₃,sᵣ)` - now it goes like `(sᵣ,s₃)(s₃,s₂)(s₂,s₁)(s₁,s₀)`

The above process can be illustrated with simple table that shows a mapping process using three rotors with only six pins which are enumerated starting from zero.

`Offsets` or `States` row is the offsets that each component have relative to the zero state (when all rotors are zeroed)

`Commutator` row marks:
  - input plate - `i`
  - ith rotor - `rᵢ`
  - reflector - `r`

`CurrentSt>` row marks current state of all rotors

The below table denotes flow of the input signal through `pin 0` of the input plate `i` which was mapped to rotor `r1` that was ticked three times. In the illustration the mapping between neighbouring rotors is linear, but in the abstract representation we do not rotate anything, but calculate that mapping with given offsets using formula `(s₁-s₀) + bᵢ`. In the first mapping between the input plate and the first rotor we substitute 3 (the state of `r₁`) in place of `s₁` and 0 (the state of input plate) in place of `s₀` and add 0 - the input pin `b₀`

```
+------------+-+-------------------+--+-------------------+--+-------------------+--+-------------------+-+
| Offsets    |0|                   |3 |                   |1 |                   |5 |                   |0|
+------------+-+-------------------+--+-------------------+--+-------------------+--+-------------------+-+
| Commutator |i|                   |r₁|                   |r₂|                   |r₃|                   |r|
+------------+-+-------------------+--+-------------------+--+-------------------+--+-------------------+-+
| CurrentSt> |0|---{(3 - 0) + 0}+->|3 |                   |5 |                   |1 |                   |0|
+------------+-+----------------|--+--+-------------------+--+-------------------+--+-------------------+-+
|            |1|                |  |4 |                   |0 |                   |2 |                   |1| 
|            | |                |  |  |                   |  |                   |  |                   | |
|            |2|     r₁ internal|  |5 |                +->|1 |--{(1 - 5) + 1}-+->|3 |                   |2| 
|            | |        mapping |  |  |     r₂ internal|  |  |                |  |  |                   | |
|            |3|                |  |0 |        mapping |  |2 |                |  |4 |                   |3| 
|            | |                |  |  |                |  |  |     r₃ internal|  |  |                   | |
|            |4|                +->|1 |--{(3 - 1) + 1}-+->|3 |        mapping |  |5 |                   |4| 
|            | |                   |  |                   |  |                |  |  |                   | |
|            |5|                   |2 |                   |4 |                +->|0 |--{(0 - 5) + 0}--->|5| 
```

### Installation

The following command will install `enigma` binary into `~/.local/bin/` directory:
```
stack install
```

### Methods of initializing the Enigma machine

`enigma` can retrieve input parameters in three ways:
  - through command line options (see below)
  - using configuration file - you simply pass a file path to `-c` option. The file must conform to the configuration file syntax (see below)
  - interactively - in case if you will use neither of the above methods, you'll be asked interactively to assign all the necessary parameters

# Command line options

The `enigma` can be run with the following command line options:
  - `-b,--bounds-info` - print max bounds of the enigma machine parameters, and other additional information
  - `-1,--first-rotor <SerialNumber>` - first-rotor serial number
  - `-2,--second-rotor <SerialNumber>` - second-rotor serial number
  - `-3,--third-rotor <SerialNumber>` - third-rotor serial number
  - `-r,--reflector <SerialNumber>` - reflectors serial number
  - `-s,--init-state <StateNumber>` - initial state of the Enigma Machine
  - `-c,--config-file <ConfigFile>` - configuration file
  - `-i,--input-file <InputFile>` - input file, default is stdin
  - `-o,--output-file <OutputFile>` - output file, default is stdout
  - `-h,--help` - show help text

### Configuration file syntax

Concrete example can be found in the `enigma.conf`

```
ConfigFile
  = Space , ("rotors" | "ro")
  , Space , Number
  , Space , Number
  , Space , Number
  , Space , ("reflector" | "re")
  , Space , Number
  , Space , ("state" | "st")
  , Space , Number
  , Space

Space = White | Comment

White = SpaceChar , {SpaceChar}

Comment = LineComment
        | BlockComment

LineComment
  = "//"
  , {<AnyExcept LineCommentTerminal>}
  , (LineCommentTerminal | <EOF>)

LineCommentTerminal = "\n"

BlockComment
  = "/*"
  , {<AnyExcept BlockCommentTerminal>}
  , BlockCommentTerminal

BlockCommentTerminal = "*/"

SpaceChar = " " | "\t" | "\n" |

Number = Digit , {Digit}

Digit = "0" .. "9"
```

### Example of usage

Encryption example of the source code for the application `./app/Main.hs` into `./Main.hs.enc`:

```
enigma -c enigma.conf -i app/Main.hs -o Main.hs.enc
```

Since the Enigma machine has a property `f(f(x)) = x`, given the same rotors, reflector and initial state it will revert encrypted text back to its initial form:
```
enigma -c enigma.conf -i Main.hs.enc -o Main.hs.dec
```
Thus output of `diff app/Main.hs Main.hs.dec` must be empty.

### Retrieving additional info about the enigma machine

The following command will print values of some constants of the enigma machine:
```
$ enigma -b
```

It will contain approximately the following information:
- Alphabet in use: `(0,255)`

- Alphabet size (n): `256`

- Max rotor serial number (n!):
```857817775342842654119082271681232625157781520279485619859655650377269452553147589377440291360451408450375885342336584306157196834693696475322289288497426025679637332563368786442675207626794560187968867971521143307702077526646451464709187326100832876325702818980773671781454170250523018608495319068138257481070252817559459476987034665712738139286205234756808218860701203611083152093501947437109101726968262861606263662435022840944191408424615936000000000000000000000000000000000000000000000000000000000000000```

- Max reflector serial number (n!/(n/2)!*2^(n/2)):
```6537256156445168127472031390784012790976081721191252400924510089038323279351219359607493746481117847060897396843121499382013181689204739112626893248251494219283641558307674830456073042673717342005838768103257372002215545075555285834707319736480712890625```

- Amount of rotors (m): `3`

- Max state number of enigma (n^m): `16777216`
