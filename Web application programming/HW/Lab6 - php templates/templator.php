<?php

/**
 * Represents the template compiler.
 */
class Templator
{
    private $lines;

    private $loadSuccessful;

    /**
     * Load a template file into memory.
     * @param string $fileName Path to the template file to be loaded.
     */
    public function loadTemplate(string $fileName)
    {
        if (!file_exists($fileName))
        {
            throw new Exception("Invalid path to file.");
        }

        $test = fopen($fileName, "r");
        while (!feof($test))
        {
            $this->lines[] = fgets($test);
        }
        fclose($test);

        $this->loadSuccessful = true;
    }

    /**
     * Compile the loaded template (transpill it into interleaved-PHP) and save the result in a file.
     * @param string $fileName Path where the result should be saved.
     */
    public function compileAndSave(string $fileName)
    {
        if (!isset($this->loadSuccessful) || !$this->loadSuccessful)
        {
            throw new Exception("Not loaded, cannot call, compileAndSave.");
        }

        $result = [];
        $stack = [];
        foreach ($this->lines as $line)
        {
            self::checkLine($line, $stack);
            $replaced = self::transform($line);
            $result[] = $replaced;
        }

        if (!empty($stack))
        {
            throw new Exception("Unpaired tag.");
        }

        self::wrireArrayToFile($result, $fileName);
    }

    private function checkLine(string $line, array &$stack)
    {
        preg_match_all("/\{(if|foreach|for) ([^{}]*)\}|\{\/(if|foreach|for)\}/", $line, $matches);
        if (!empty($matches[0]))
        {
            foreach ($matches[1] as $index => $opening)
            {
                if (!empty($opening))
                {
                    $expression = $matches[2][$index];
                    if (empty($expression))
                    {
                        throw new Exception("Empty string is invalid in expr and cond.");
                    }
                    $stack[] = $opening;
                }
                else
                {
                    $closing = $matches[3][$index];
                    $top_stack = array_pop($stack);
                    if ($top_stack != $closing)
                    {
                        throw new Exception("Crossed loops.");
                    }
                }
            }
        }
    }

    private function transform(string $line) : string
    {
        $replaced = $line;
        // Replace expressions - might be missing some checks here.
        $replaced = preg_replace("/\{= ([^{}]*)\}/", "<?= htmlspecialchars($1) ?>", $replaced);
        // Replace openings.
        $replaced = preg_replace("/\{(if|foreach|for) ([^{}]*)\}/", "<?php $1 ($2) { ?>", $replaced);
        // Replace endings.
        $replaced = preg_replace("/\{\/(if|foreach|for)\}/", "<?php } ?>", $replaced);

        return $replaced;
    }

    private function wrireArrayToFile(array $array, string $fileName)
    {
        $file = fopen($fileName, "w");
        if (!$file)
        {
            throw new Exception("Unable to create the file.");
        }
        foreach ($array as $value)
        {
            fwrite($file, $value);
        }
        fclose($file);
    }
}
