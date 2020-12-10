test_us_tax_code:
	@SCOPE="TestSection132_1" $(MAKE) --no-print-directory -C us_tax_code tests/master_section_132.run \
	> /dev/null || { echo "[FAIL us_tax_code/section_132.catala_en (1/3)]"; exit 1; }
	@echo "[PASS us_tax_code/section_132.catala_en (1/3)]"
	@SCOPE="TestSection132_2" $(MAKE) --no-print-directory -C us_tax_code tests/master_section_132.run \
	> /dev/null || { echo "[FAIL us_tax_code/section_132.catala_en (1/3)]"; exit 1; }
	@echo "[PASS us_tax_code/section_132.catala_en (2/3)]"
	@SCOPE="TestSection132_3" $(MAKE) --no-print-directory -C us_tax_code tests/master_section_132.run \
	> /dev/null || { echo "[FAIL us_tax_code/section_132.catala_en (1/3)]"; exit 1; }
	@echo "[PASS us_tax_code/section_132.catala_en (3/3)]"

test_examples: test_us_tax_code