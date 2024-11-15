import * as api from "../api-types";

export type State = { loading: true } | { tokenize: Tokenize };

export type Tokenize = {
  readTh: api.ReadTh;
};
