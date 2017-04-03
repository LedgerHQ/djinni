// AUTOGENERATED FILE - DO NOT MODIFY!
// This file generated by Djinni from example.djinni

#import "TXSItemList.h"
#import "TXSSortOrder.h"
#import <Foundation/Foundation.h>
@class TXSSortItems;
@protocol TXSTextboxListener;
@protocol TXSTextboxResetListener;


@interface TXSSortItems : NSObject

/** For the iOS / Android demo */
- (void)sort:(TXSSortOrder)order
       items:(nonnull TXSItemList *)items;

- (void)reset;

+ (nullable TXSSortItems *)createWithListener:(nullable id<TXSTextboxListener>)listener
                                resetListener:(nullable id<TXSTextboxResetListener>)resetListener;

/** For the localhost / command-line demo */
+ (nonnull TXSItemList *)runSort:(nonnull TXSItemList *)items;

- (int32_t)count;

@end
