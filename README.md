# Enigma machine model

### Description
Enigma is a command line program that (relatively inefficiently) encrypts your data either from a file or from `<stdin>`, and writes its output to a specified file or to `<stdout>`. It does not abide by the constraint of 26 symbols from the original hardware implementation, and currently it maps not the characters, but the bytes that represent them, so it's capable of encrypting text constituted with any set of characters.

### Installation
The following command will install `enigma` binary into `~/.local/bin/` directory:
```
stack install
```

### Options
It is supposed to be executed in the following way:
```
$ enigma <RequiredArgs> [Options]
```
Options are defined like this:
  - `-h,--help` - print help message and exit
  - `-b,--bounds` - print max bounds of the enigma machine parameters,
          and other additional information and exit
  - `-i<InpFile>, --input=<InpFile>` - input file, default is `<stdin>`
  - `-o<OutFile>, --output=<OutFile>` - output file, default is `<stdout>`

It is required to pass enigma machine parameters explicitly:
  - `-r<n1>,<n2>,<n3>, --rotors=<n1>,<n2>,<n3>` - serial numbers for rotors
  - `-e<num>, --reflector=<num>` - serial number for the reflector
  - `-s<num>, --state=<num>` - initial state of the enigma machine


### Example of usage

Encryption example of the source code for the application `./app/Main.hs` into `./Main.hs.enc`:
```
enigma \
  -r1231283102938129031231239128319\  # 1st rotor serial number
   ,9123812398123091823\              # 2nd rotor serial number
   ,12938123918239128312391289\       # 3rd rotor serial number
  -e1920381230981209820984109238192\  # reflector serial number
  -s831\                              # initial state of enigma
  -i./app/Main.hs\                    # input file
  -o./Main.hs.enc\                    # output file
```

Given the same rotors, reflector and initial state it will revert encrypted text back to its initial form:
```
enigma \
  -r1231283102938129031231239128319\  # 1st rotor serial number
   ,9123812398123091823\              # 2nd rotor serial number
   ,12938123918239128312391289\       # 3rd rotor serial number
  -e1920381230981209820984109238192\  # reflector serial number
  -s831\                              # initial state of enigma
  -iMain.hs.enc\                      # input file
                                      # output goes to stdout
```

So, the property `f(f(x)) = x` should hold for every text whose elements belong to the supported alphabet.

### Retrieving additional info about the enigma machine
The following command will print values of some constants of the enigma machine:
```
$ enigma -b
```

It will contain approximately the following information:
1. Alphabet in use: `(0,255)`

2. Alphabet size (n): `256`

3. Max rotor serial number (n!):
```857817775342842654119082271681232625157781520279485619859655650377269452553147589377440291360451408450375885342336584306157196834693696475322289288497426025679637332563368786442675207626794560187968867971521143307702077526646451464709187326100832876325702818980773671781454170250523018608495319068138257481070252817559459476987034665712738139286205234756808218860701203611083152093501947437109101726968262861606263662435022840944191408424615936000000000000000000000000000000000000000000000000000000000000000```

4. Max reflector serial number (n!/(n/2)!*2^(n/2)):
```6537256156445168127472031390784012790976081721191252400924510089038323279351219359607493746481117847060897396843121499382013181689204739112626893248251494219283641558307674830456073042673717342005838768103257372002215545075555285834707319736480712890625```

5. Amount of rotors (m): `3`

6. Max state number of enigma (n^m): `16777216`
