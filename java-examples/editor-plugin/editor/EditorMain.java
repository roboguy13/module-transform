package editor;

import javax.swing.*;

import file.File;
import network.Network;
import logger.Logger;
import plugin.Plugin;

import java.awt.*;
import java.awt.event.*;

public class EditorMain {
    private static void createAndShowGUI() {
        File file = new File();
        Network network = new Network();

        JFrame frame = new JFrame("Editor");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

        JMenuBar menuBar = new JMenuBar();

        JMenu menuFile = new JMenu("File");
        JMenuItem menuItemFileOpen = new JMenuItem("Open");
        menuItemFileOpen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                file.open();
            }
        });
        menuFile.add(menuItemFileOpen);
        JMenuItem menuItemFilePlugin = new JMenuItem("Plugin");
        menuItemFilePlugin.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                Logger<File> fileLogger = new Logger<File>(file);
                Plugin<File> filePlugin = new Plugin<File>(fileLogger);
                filePlugin.run();
            }
        });
        menuFile.add(menuItemFilePlugin);
        menuBar.add(menuFile);

        JMenu menuNetwork = new JMenu("Network");
        JMenuItem menuItemNetworkOpen = new JMenuItem("Open");
        menuItemNetworkOpen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                network.open();
            }
        });
        menuNetwork.add(menuItemNetworkOpen);
        JMenuItem menuItemNetworkPlugin = new JMenuItem("Plugin");
        menuItemNetworkPlugin.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                Logger<Network> networkLogger = new Logger<Network>(network);
                Plugin<Network> networkPlugin = new Plugin<Network>(networkLogger);
                networkPlugin.run();
            }
        });
        menuNetwork.add(menuItemNetworkPlugin);
        menuBar.add(menuNetwork);

        frame.getContentPane().add(menuBar, BorderLayout.PAGE_START);

        JTextPane textPane = new JTextPane();
        textPane.setPreferredSize(new Dimension(500, 500));
        frame.getContentPane().add(textPane, BorderLayout.CENTER);

        frame.pack();
        frame.setVisible(true);
    }

    public EditorMain() {
        javax.swing.SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                createAndShowGUI();
            }
        });
    }
}
