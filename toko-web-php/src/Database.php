<?php

final class Database
{
    private PDO $pdo;

    public function __construct(string $path)
    {
        $dir = dirname($path);
        if (!is_dir($dir)) {
            mkdir($dir, 0777, true);
        }

        $this->pdo = new PDO('sqlite:' . $path);
        $this->pdo->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);
        $this->pdo->setAttribute(PDO::ATTR_DEFAULT_FETCH_MODE, PDO::FETCH_ASSOC);
    }

    public function pdo(): PDO
    {
        return $this->pdo;
    }

    public function migrate(string $schemaPath): void
    {
        $this->pdo->exec(file_get_contents($schemaPath));
        $this->seed();
    }

    private function seed(): void
    {
        $cashiers = [
            ['01', '1', 'ROYANI', 63],
            ['02', '2', 'MAKSUM', 26],
            ['03', '3', 'RIZQIFAUZI', 3],
            ['04', '4', 'ZAM-ZAMI', 0],
        ];

        $stmt = $this->pdo->prepare(
            'INSERT OR IGNORE INTO cashiers (legacy_no, code, name, password, last_note) VALUES (?, ?, ?, ?, ?)'
        );

        foreach ($cashiers as [$legacyNo, $code, $name, $lastNote]) {
            $stmt->execute([$legacyNo, $code, $name, '00', $lastNote]);
        }

        $registers = [
            ['1', 'A', 58],
            ['2', 'A', 58],
            ['3', 'A', 50],
        ];

        $stmt = $this->pdo->prepare(
            'INSERT OR IGNORE INTO registers (register_no, mode, receipt_width) VALUES (?, ?, ?)'
        );

        foreach ($registers as [$registerNo, $mode, $width]) {
            $stmt->execute([$registerNo, $mode, $width]);
        }

        $products = [
            ['BN', '00', 'BERAS SPHP / KG', 'PCS', -606, 13500, 14000],
            ['8991906106311', '00', 'APEL ROYAL', 'PCS', -3, 15000, 15500],
            ['7118441200327', '11', 'ABC SAMBAL ASLI 135ML', 'PCS', 0, 6200, 6500],
        ];

        $stmt = $this->pdo->prepare(
            'INSERT OR IGNORE INTO products (barcode, group_code, name, unit, stock, cost_price, retail_price) VALUES (?, ?, ?, ?, ?, ?, ?)'
        );

        foreach ($products as $product) {
            $stmt->execute($product);
        }
    }
}

