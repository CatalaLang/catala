var Law = require("./french_law.js");
var Benchmark = require("benchmark");
var suite = new Benchmark.Suite();

suite
  .add("AllocationFamiliales#benchmark", function () {
    Law.computeAllocationsFamiliales({
      currentDate: new Date("2020-05-20"),
      children: [
        {
          id: 0,
          remunerationMensuelle: 0,
          dateNaissance: new Date("2003-03-02"),
          gardeAlternee: false,
          priseEnChargeServiceSociaux: false,
        },
        {
          id: 1,
          remunerationMensuelle: 300,
          dateNaissance: new Date("2013-10-30"),
          gardeAlternee: true,
          gardeAlterneePartageAllocation: true,
          priseEnChargeServiceSociaux: false,
        },
      ],
      income: 30000,
      residence: "Métropole",
    });
  })
  .on("cycle", function (event) {
    console.log(String(event.target));
  })
  .on("complete", function () {
    console.log("Fastest is " + this.filter("fastest").map("name"));
  })
  .run({ async: true });
