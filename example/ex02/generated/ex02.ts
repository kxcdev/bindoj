export type ProductId = number;

export type ProductDetails = { count: number; description: string; name: string; price: number };

export type Product = { details: ProductDetails; id: ProductId };

export type OrderId = number;

export type PaymentMethod =
  | { kind: "bank-transfer"; accountNumber: string; bankName: string; holderName: string }
  | {
      kind: "credit-card";
      cardNumber: string;
      cvv: string;
      expirationDate: { _0: number; _1: number };
      holderName: string;
    };
export function analyzePaymentMethod<__bindoj_ret>(__bindoj_fns: {
  "bank-transfer": (__bindoj_v: {
    kind: "bank-transfer";
    accountNumber: string;
    bankName: string;
    holderName: string;
  }) => __bindoj_ret;
  "credit-card": (__bindoj_v: {
    kind: "credit-card";
    cardNumber: string;
    cvv: string;
    expirationDate: { _0: number; _1: number };
    holderName: string;
  }) => __bindoj_ret;
}): (__bindoj_x: PaymentMethod) => __bindoj_ret {
  return (__bindoj_x: PaymentMethod) => {
    if (__bindoj_x.kind === "bank-transfer") {
      return __bindoj_fns[__bindoj_x.kind](__bindoj_x);
    } else if (__bindoj_x.kind === "credit-card") {
      return __bindoj_fns[__bindoj_x.kind](__bindoj_x);
    } else {
      throw new TypeError("panic @analyzePaymentMethod - unrecognized: " + __bindoj_x);
    }
  };
}

export type OrderDetails = { paymentMethod: PaymentMethod; products: { _0: ProductId; _1: number }[] };

export type OrderStatus = "Unpaid" | "Paid" | "Shipped" | "Delivered" | "Canceled";

export type Order = { details: OrderDetails; id: OrderId; status: OrderStatus; totalPrice: number };

export type ProductQuery = {
  limit: number | null | undefined;
  maximumPrice: number | null | undefined;
  minimumPrice: number | null | undefined;
  searchQuery: string | null | undefined;
};

export type OrderQuery = {
  limit: number | null | undefined;
  maximumPrice: number | null | undefined;
  minimumPrice: number | null | undefined;
  products: number[] | null | undefined;
  status: OrderStatus[] | null | undefined;
};
