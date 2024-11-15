export type ReadTh = {
  rthName: string;
  /** UTCDate */
  rthLastView: string;
};

export type ReadThs = [number, ReadTh][];

export type Choice = {
  choText: string;
  choRest: string[];
};
