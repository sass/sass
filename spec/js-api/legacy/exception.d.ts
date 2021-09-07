export interface SassException extends Error {
  message: string;
  formatted: string;
  line: number;
  column: number;
  status: number;
  file: string;
}
