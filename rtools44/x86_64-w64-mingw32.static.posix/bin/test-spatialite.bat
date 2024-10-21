REM run against a database that should not exist, but will be removed afterward to save space.
test-spatialite.exe test-db.sqlite
del test-db.sqlite
