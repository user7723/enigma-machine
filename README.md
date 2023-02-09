# Enigma Model

### Description
Enigma is a command line program that (inefficiently) encrypts your data either from a file or from `<stdin>`, and writes its output to a specified file or to `<stdout>`. It does not abide by the constraint of 26 symbols from the original hardware implementation, and it's capable to map all ASCII characters from printable range except for `delete` character which was replaced with `newline` character.

It is supposed to be executed in the following way:
```
$ enigma <RequiredArgs> [Options]
```
### Options
Where options are defined like this:
  - `-h,--help` - print this help message
  - `-b,--bounds` - print max bounds of the enigma machine parameters,
          and other additional information
  - `-i<InpFile>, --input=<InpFile>` - input file, default is `<stdin>`
  - `-o<OutFile>, --output=<OutFile>` - output file, default is `<stdout>`

It is required to pass enigma machine parameters explicitly:
  - `-r<n1>,<n2>,<n3>, --rotors=<n1>,<n2>,<n3>` - serial numbers for rotors
  - `-e<num>, --reflector=<num>` - serial number for the reflector
  - `-s<num>, --state=<num>` - initial state of the enigma machine


### Example of usage

Example of encryption of the source code for the application `./app/Main.hs` into `./Main.hs.enc`:
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

So, the property `f(f(x)) = x` should hold for every text whose elements are belong to the supported alphabet.

### Retrieving additional info about the enigma machine
The following command will tell about values of some constants of the enigma machine:
```
$ enigma -b
```

It will contain approximately the following information:

1. Alphabet in use:
```" !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\n"```

2. Alphabet size (n): `96`

3. Max rotor serial number (n!):
`991677934870949689209571401541893801158183648651267795444376054838492222809091499987689476037000748982075094738965754305639874560000000000000000000000`

4. Max reflector serial number (n!/(n/2)!*2^(n/2)):
`283806325080779912837729172696128150920628587998105114415737667754150390625`

5. Amount of rotors (m): `3`

6. Max state number of enigma (n^m): `884736`
