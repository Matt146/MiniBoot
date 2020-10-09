# MiniBoot

MiniBoot is a minimalistic x86 bootloader that can load flat binaries of kernels and switch to protected mode. It was originally intended to be used for my OS, MattOS, but since then, I've decided to write a bootloader that can boot into long mode instead, to get the full 64-bit functionality of my CPU.

# How it works
1. MiniBoot uses the extended read BIOS interrupt (int 0x13, ah=0x42) in order to read the next 15 sectors after the MBR, where your kernel must be located and loads them at the physical address, 0x8c00
2. It then switches to protected mode
3. Next, it jumps to kernel address at 0x8c00
4. Note that it uses a 32-bit flat memory model for simplicity
5. Also, note that MiniBoot requires a flat binary format for the kernel
