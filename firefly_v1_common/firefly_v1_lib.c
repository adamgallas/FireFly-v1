#include "stdio.h"
#include "unistd.h"
#include "sys/mman.h"
#include "sys/time.h"
#include "fcntl.h"

#define FIREFLY_CTRL_IO 0x400000000
#define FIREFLY_CTRL_IO_SPACE (1 << 10)

typedef unsigned int u32;
typedef unsigned char u8;

void sub_test(u32 *reg)
{
    for (u32 i = 0; i < 16; i++)
    {
        printf("%d ", reg[i]);
    }
    printf("\n");
    return;
}

void test(u32 *reg[], u32 length)
{
    for (u32 i = 0; i < length; i++)
    {
        sub_test(reg[i]);
    }
    return;
}

void single_layer_schedule(u32 *volatile ptr, u32 *reg)
{

    ptr[0] = reg[0];
    ptr[1] = reg[1];
    ptr[2] = reg[2];
    ptr[3] = reg[3];

    ptr[8] = reg[4];
    ptr[9] = reg[5];
    ptr[10] = reg[6];
    ptr[11] = reg[7];
    ptr[12] = reg[8];
    ptr[13] = reg[9];
    ptr[14] = reg[10];
    ptr[15] = reg[11];
    ptr[16] = reg[12];
    ptr[17] = reg[13];

    ptr[18] = reg[14];
    ptr[18] = reg[14];
    ptr[18] = reg[15];

    while (ptr[6] == 0)
        ;
    ptr[6] = 0;
    return;
}

void firefly_v1_schedule(u32 *reg[], u8 length)
{

    int fd = open("/dev/mem", O_RDWR | O_SYNC);
    u32 *volatile ptr = (u32 *)mmap(
        0,
        FIREFLY_CTRL_IO_SPACE,
        PROT_READ | PROT_WRITE,
        MAP_SHARED,
        fd,
        FIREFLY_CTRL_IO);

    for (u8 i = 0; i < length; i++)
    {
        single_layer_schedule(ptr, reg[i]);
    }
    close(fd);
    munmap(ptr, FIREFLY_CTRL_IO_SPACE);
    return;
}

void firefly_v1_schedule_time_it(u32 *reg[], u8 length)
{
    struct timeval st, et;

    int fd = open("/dev/mem", O_RDWR | O_SYNC);
    u32 *volatile ptr = (u32 *)mmap(
        0,
        FIREFLY_CTRL_IO_SPACE,
        PROT_READ | PROT_WRITE,
        MAP_SHARED,
        fd,
        FIREFLY_CTRL_IO);

    gettimeofday(&st, NULL);
    for (u8 i = 0; i < length; i++)
    {
        single_layer_schedule(ptr, reg[i]);
    }
    gettimeofday(&et, NULL);
    close(fd);
    munmap(ptr, FIREFLY_CTRL_IO_SPACE);

    int elapsed = ((et.tv_sec - st.tv_sec) * 1000000) + (et.tv_usec - st.tv_usec);
    printf("Pure Inference time: %d micro seconds\n", elapsed);
    return;
}
