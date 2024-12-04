#import <GNUstepBase/GNUstep.h>

int main(int argc, const char * argv[]) {
    NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init]; // Create autorelease pool
    NSLog(@"Hello, GNUstep!"); // Log a message
    [pool drain]; // Release the autorelease pool
    return 0;
}
