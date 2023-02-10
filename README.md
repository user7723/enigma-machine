# Enigma machine model

### Description
Enigma is a command line program that (very inefficiently) encrypts your data either from a file or from `<stdin>`, and writes its output to a specified file or to `<stdout>`. It does not abide by the constraint of 26 symbols from the original hardware implementation, and it's capable to map all characters from the ASCII range. All the characters that fall out of the supported range will simply filtered out from the input.

It is supposed to be executed in the following way:
```
$ enigma <RequiredArgs> [Options]
```
### Options
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
1. Alphabet in use:
```"\NUL\SOH\STX\ETX\EOT\ENQ\ACK\a\b\t\n\v\f\r\SO\SI\DLE\DC1\DC2\DC3\DC4\NAK\SYN\ETB\CAN\EM\SUB\ESC\FS\GS\RS\US !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\DEL\128\129\130\131\132\133\134\135\136\137\138\139\140\141\142\143\144\145\146\147\148\149\150\151\152\153\154\155\156\157\158\159\160\161\162\163\164\165\166\167\168\169\170\171\172\173\174\175\176\177\178\179\180\181\182\183\184\185\186\187\188\189\190\191\192\193\194\195\196\197\198\199\200\201\202\203\204\205\206\207\208\209\210\211\212\213\214\215\216\217\218\219\220\221\222\223\224\225\226\227\228\229\230\231\232\233\234\235\236\237\238\239\240\241\242\243\244\245\246\247\248\249\250\251\252\253\254\255"```

2. Alphabet size (n): `256`

3. Max rotor serial number (n!):
```857817775342842654119082271681232625157781520279485619859655650377269452553147589377440291360451408450375885342336584306157196834693696475322289288497426025679637332563368786442675207626794560187968867971521143307702077526646451464709187326100832876325702818980773671781454170250523018608495319068138257481070252817559459476987034665712738139286205234756808218860701203611083152093501947437109101726968262861606263662435022840944191408424615936000000000000000000000000000000000000000000000000000000000000000```

4. Max reflector serial number (n!/(n/2)!*2^(n/2)):
```6537256156445168127472031390784012790976081721191252400924510089038323279351219359607493746481117847060897396843121499382013181689204739112626893248251494219283641558307674830456073042673717342005838768103257372002215545075555285834707319736480712890625```

5. Amount of rotors (m): `3`

6. Max state number of enigma (n^m): `16777216`
