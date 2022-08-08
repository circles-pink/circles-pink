import crypto from "crypto";

const exampleSecretKey = "QTbY4bCBk8DByPqXzvib7tEwBzQdSYDo";

export const encrypt = (secretKey: string, data: string) => {
  const cipher = crypto.createCipheriv("AES-256-ECB", secretKey, null);
  let encrypted = cipher.update(data, "utf8", "base64");
  encrypted += cipher.final("base64");
  return encrypted;
};

export const decrypt = (secretKey: string, data: string) => {
  const decipheriv = crypto.createDecipheriv("AES-256-ECB", secretKey, null);
  let decryptediv = decipheriv.update(data, "base64", "utf8");
  decryptediv += decipheriv.final("utf8");
  return decryptediv;
};

const encrypted = encrypt(exampleSecretKey, "Encrypt-Me :-)");
console.log("encrypted: ", encrypted);
console.log("decrypted: ", decrypt(exampleSecretKey, encrypted));
