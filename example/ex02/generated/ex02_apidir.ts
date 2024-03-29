import { apidir as bindoj } from "../../../with_js/public-packages/runtime/index";
import { Order, OrderDetails, OrderId, OrderQuery, Product, ProductDetails, ProductId, ProductQuery } from "./ex02";

export const Ex02InvpInfo = {
  "get-products": {
    name: "get-products",
    method: "POST",
    urlpath: "/products/get",
    requestType: undefined as unknown as ProductQuery,
    responseType: undefined as unknown as Product[],
  },
  "get-orders": {
    name: "get-orders",
    method: "POST",
    urlpath: "/orders/get",
    requestType: undefined as unknown as OrderQuery,
    responseType: undefined as unknown as Order[],
  },
  "get-product": {
    name: "get-product",
    method: "POST",
    urlpath: "/product/get",
    requestType: undefined as unknown as ProductId,
    responseType: undefined as unknown as Product | string,
  },
  "get-order": {
    name: "get-order",
    method: "POST",
    urlpath: "/order/get",
    requestType: undefined as unknown as OrderId,
    responseType: undefined as unknown as Order | string,
  },
  "register-product": {
    name: "register-product",
    method: "POST",
    urlpath: "/product/register",
    requestType: undefined as unknown as ProductDetails,
    responseType: undefined as unknown as ProductId,
  },
  "register-order": {
    name: "register-order",
    method: "POST",
    urlpath: "/order/register",
    requestType: undefined as unknown as OrderDetails,
    responseType: undefined as unknown as OrderId | string | string,
  },
  "update-product-details": {
    name: "update-product-details",
    method: "POST",
    urlpath: "/product/details/update",
    requestType: undefined as unknown as { _0: ProductId; _1: ProductDetails },
    responseType: undefined as unknown as OrderId | string,
  },
  "update-order-status": {
    name: "update-order-status",
    method: "POST",
    urlpath: "/order/status/update",
    requestType: undefined as unknown as { _0: OrderId; _1: "Unpaid" | "Paid" | "Shipped" | "Delivered" | "Canceled" },
    responseType: undefined as unknown as OrderId | string,
  },
} as const;
export type Ex02InvpInfoMap = bindoj.IsApiDirInfoMap<typeof Ex02InvpInfo>;
export type Ex02ClientIntf = bindoj.ApiDirClientPromiseIntf<Ex02InvpInfoMap>;
