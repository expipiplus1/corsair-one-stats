# Corsair One Stats

A program to print out the fan speed, pump speeds and coolant temperatures for
the Corsair One computer. For example:

```
Fan Speed   : 153 RPM
CPU Pump    : 1864 RPM
GPU Pump    : 1972 RPM
CPU Coolant : 41.84 °C
GPU Coolant : 39.99 °C
```

Development was done by reverse engineering the usb traffic sent from the
"iCue" software running in a Windows 10 VM.

## *Important Notes*

In developing this I managed to get my fan stuck at 173 RPM, not even power cycling can fix
this. Disconnecting the PWM pin on the fan has made the computer usable again
by causing the fan to run at full speed, oops!

Please keep a careful eye on the component temperatures while running this! I
have only an educated guess about what it's doing! I'm pretty sure that it's
just fetching data and isn't influencing any state on the device, but who knows
for sure.

Note that the system will stop the fan when things are cool, i.e. 0 RPM is a
valid speed.

## The protocol

The core of things is that the host sends a 64B `SET_REPORT` packet, asking for
report type `2`, id `0`. This packet always starts with the byte `0x3f` (packet length probably), then a
counter which *tends* to increase by about 8 each time. The rest of the payload
is filled with some "low entropy" bytes, i.e. all zeros, or incrementing values
(the alphabet appears sometimes) or all 0xff or 0x7f. It ends in a CRC-8
checksum of the counter and the garbage bytes (everything except the 0x3f).

The response comes as a 64B `URB_INTERRUPT` packet. The speeds are in RPM and
the temperatures are in 256ths of a degree, all 2B in size

| Data        | Unit  | offset |
|-------------|-------|--------|
| GPU coolant | C/256 | 0x07   |
| Fan speed   | RPM   | 0x0f   |
| GPU pump    | RPM   | 0x16   |
| CPU pump    | RPM   | 0x1d   |
| CPU coolant | C/256 | 0x21   |

There is some other information in the packet:

- some bytes which all tend to have the same value for a particular packet,
  perhaps the age of the data.
- some bytes which are always the same, perhaps the bounds of the data.

The device causes `-EPIPE` when `SET_IDLE` is sent. This happens for iCue, so
we do the same and ignore the error.

## Our operation

Currently this program just replicates the first request packet that iCue
sends: [0x3f, 0x10, 0xff, 0x00 .. 0x00, 0x5b], this works well enough.

Querying too fast causes
- Correct results for a few seconds
- Garbage(?) results for a few seconds
- Device stops responding until a power cycle, not even iCue can revive it

From time to time the device gets into a bad state and returns 64B packets I
have not deciphered. This can be fixed (in order of descending hassle and
ascending effectiveness)

- Resetting the device from Linux
    - `echo 0 > /sys/bus/usb/devices/1-7/authorized && echo 1 > /sys/bus/usb/devices/1-7/authorized`
    - Make sure `1-7` is the correct device!
- Running iCue with the device connected
- Power cycling the device
  - `rtcwake --seconds 6; systemctl suspend`

## See also

It seems that the `vendor:product` id matches that of the "H110i Pro" which is
supported by [OpenCorsairLink](https://github.com/audiohacked/OpenCorsairLink).
Despite being the same USB device the firmware is different and it is not
compatible with OpenCorsairLink (see
[here](https://github.com/audiohacked/OpenCorsairLink/issues/220#issue-543176212)
and
[here](https://github.com/audiohacked/OpenCorsairLink/issues/160#issue-425542928)).
Having said that a couple of people [have had success with
OpenCorsairLink](https://github.com/audiohacked/OpenCorsairLink/issues/109#issuecomment-435685582),
this didn't seem to work on my machine.

