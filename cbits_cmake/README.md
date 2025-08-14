# Developer note

Consider using this following command in your workflow:
```bash
mkdir -p build/
cd build/
cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 -G Ninja .. && cmake --build . && cp compile_commands.json ../../compile_commands.json
```