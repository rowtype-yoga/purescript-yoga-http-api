const canceler = (_cancelError, _onError, onSuccess) => onSuccess();

export const collectImpl = (stream) => (onError, onSuccess) => {
  const reader = stream.getReader();
  const values = [];
  const pump = () => {
    reader.read().then(
      ({ done, value }) => {
        if (done) {
          reader.releaseLock();
          onSuccess(values);
        } else {
          values.push(value);
          pump();
        }
      },
      onError,
    );
  };
  pump();
  return canceler;
};

export const readOneAndCancelImpl = (stream) => (onError, onSuccess) => {
  const reader = stream.getReader();
  reader.read().then(
    ({ done, value }) => {
      if (done) {
        onError(new Error("stream closed before its first chunk"));
        return;
      }
      reader.cancel("test complete").then(
        () => {
          reader.releaseLock();
          onSuccess(value);
        },
        onError,
      );
    },
    onError,
  );
  return canceler;
};
