export const uorToMaybeImpl = (just) => (nothing) => (value) => {
  return value === undefined ? nothing : just(value);
};
