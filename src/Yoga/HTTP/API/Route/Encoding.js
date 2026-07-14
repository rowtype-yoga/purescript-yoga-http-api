export const multipartFileBytes = (value) => value;

export const textChunk = (value) => value;
export const binaryChunk = (value) => value;

export const streamingImpl = (pull, cancel) =>
  new ReadableStream({
    async pull(controller) {
      const chunk = await pull();
      if (chunk == null) {
        controller.close();
      } else {
        controller.enqueue(chunk);
      }
    },
    cancel() {
      cancel();
    },
  });
