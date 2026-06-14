<?php

final class LegacyRules
{
    public static function registerNo(): string
    {
        if (!empty($_SESSION['register_no'])) {
            return (string) $_SESSION['register_no'];
        }

        $fromEnv = getenv('KASSA');
        if ($fromEnv !== false && trim($fromEnv) !== '') {
            $_SESSION['register_no'] = trim($fromEnv);
            return $_SESSION['register_no'];
        }

        $_SESSION['register_no'] = '1';
        return '1';
    }

    public static function formatRupiah(int|float $value): string
    {
        return number_format((float) $value, 0, ',', '.');
    }

    public static function nextSaleNo(PDO $pdo, string $registerNo, int $cashierId): string
    {
        $stmt = $pdo->prepare('SELECT last_note FROM cashiers WHERE id = ?');
        $stmt->execute([$cashierId]);
        $lastNote = (int) ($stmt->fetchColumn() ?: 0);
        $next = $lastNote + 1;

        if ($next > 99999) {
            $next = 1;
        }

        return sprintf('%05d-%s', $next, $registerNo);
    }

    public static function roundTotal(int $total): array
    {
        $rounded = (int) (round($total / 100) * 100);
        return [$rounded, $rounded - $total];
    }
}

