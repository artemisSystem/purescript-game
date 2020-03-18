exports.mkRunGameImpl =
  runUpdate => interpret => parallellize => reduce => updates =>
    parallellize(
      updates
        // The undefined arguments being passed here are dictionaries for the
        // Union typeclass. They are never used and are passed like this in
        // generated code as well.
        .map(runUpdate()()( reduce()() ))
        .map(interpret)
    );