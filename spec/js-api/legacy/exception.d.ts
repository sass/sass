export interface LegacyException extends Error {
  message: string;

  formatted: string;

  line?: number;

  column?: number;

  status: number;

  file?: string;
}
