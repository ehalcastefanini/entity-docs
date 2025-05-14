<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRconsStandardLogisticCosts`

## 1. Overview:

### Objective and Problem Solved:
The `FRconsStandardLogisticCosts` unit is designed to manage and display standard logistic costs in a grid-based interface. It provides functionalities for adding, deleting, and editing logistic cost records, as well as performing specific actions like loading data and calculating logistics statistics. The primary problem it solves is the efficient management and visualization of logistic cost data, including warehouse and shipping method details.

### Technologies Used:
- **Delphi VCL Framework**: For building the user interface and handling events.
- **SOAP Services**: For interacting with external services like `ConsigneeServiceUtils`, `WarehouseNoDetailsServiceUtils`, and `ShippingServiceUtils`.
- **TClientDataSet**: For managing in-memory datasets.
- **cxGrid**: For displaying and managing grid-based data.
- **ActionList**: For managing user actions like "Add," "Delete," and "Load."

### Form Type:
This is a **grid display** form.

#### Grid Columns and Their Types:
1. **stat**: Status (string).
2. **whse**: Warehouse code (string).
3. **whseName**: Warehouse name (string).
4. **shipMthd**: Shipping method code (string).
5. **shipMthdName**: Shipping method name (string).
6. **payload**: Payload (numeric).
7. **stdCost**: Standard cost (numeric).

#### Grid Actions and Their Effects:
1. **Add**: Adds a new record to the grid.
2. **Delete**: Deletes the selected record from the grid.
3. **Loads**: Executes a specific action to load data into the grid.

---

## 2. Functionality Description:

### User/Software Actions:
1. Add new logistic cost records.
2. Delete existing records.
3. Load data into the grid.
4. Edit specific fields in the grid (e.g., warehouse, shipping method).
5. Calculate logistics statistics.

### Main Components:
- **Grid (`cxGrid`)**: Displays logistic cost data.
- **Action Buttons**: Includes "Add," "Delete," and "Loads" buttons.
- **ClientDataSet (`CDStable`)**: Manages the data displayed in the grid.
- **SOAP Services**: Interacts with external services for data retrieval and updates.

### Pseudo-code for Actions and Events:
- `OnEditValueChanged` event of a grid cell: `if cell value changed then validate and update record`.
- `OnNewRecord` event of the dataset: `if new record created then initialize default values`.
- `OnExecute` event of "Add" action: `if Add button clicked then create new record`.
- `OnExecute` event of "Delete" action: `if Delete button clicked then remove selected record`.
- `OnExecute` event of "Loads" action: `if Loads button clicked then load data into grid`.

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The frame is initialized with default settings, including grid configurations and SOAP service setup.
   - Hidden fields, column order, and key fields are defined for the grid.

2. **User Interactions**:
   - Users can interact with the grid to edit values or perform actions using buttons.
   - Buttons trigger specific actions like adding, deleting, or loading data.

3. **Functions and File Locations**:
   - `cxDBVtableEditValueChanged` (in this unit): Handles cell value changes.
   - `ACTaddExecute` (in this unit): Adds a new record.
   - `ACTdeleteExecute` (in this unit): Deletes a selected record.
   - `ACTloadsExecute` (in this unit): Loads data into the grid.
   - `m_CalcLogCostsStat` (in this unit): Calculates logistics statistics.

### Required Data:
- Users must provide values for fields like `whse`, `shipMthd`, `payload`, and `stdCost` to add or edit records.

---

## 4. Business Rules:

### Actions and Preconditions:
1. **Add**: Enabled when the user clicks the "Add" button.
2. **Delete**: Enabled only when a record is selected in the grid.
3. **Loads**: Executes without preconditions.

### Available Filters:
- No explicit filters are defined in the code.

### Error Messages:
- Not explicitly defined in the code.

### Default Field Values:
- Not explicitly defined in the code.

### Field Validation and Conditions:
- **whse**: Must be a valid warehouse code.
- **shipMthd**: Must be a valid shipping method code.
- **payload**: Should be numeric.
- **stdCost**: Should be numeric.

---

## 5. Main Functions:

1. **`m_FindByCodeShipMethod`**: Finds a shipping method by code.
2. **`m_FindByCodeWhse`**: Finds a warehouse by code.
3. **`m_FindServiceShipMethod`**: Handles the selection of a shipping method.
4. **`m_FindWhse`**: Handles the selection of a warehouse.
5. **`m_CalcLogCostsStat`**: Calculates logistics statistics for a given consignee code.

---

## 6. API Service Consumption:

### External Service Calls:
1. **Service Name**: `ConsigneeServiceUtils`
   - **Endpoint**: Not explicitly defined.
   - **Purpose**: Provides consignee-related data.
2. **Service Name**: `WarehouseNoDetailsServiceUtils`
   - **Endpoint**: Not explicitly defined.
   - **Purpose**: Provides warehouse-related data.
3. **Service Name**: `ShippingServiceUtils`
   - **Endpoint**: Not explicitly defined.
   - **Purpose**: Provides shipping method-related data.

---

## 7. Conditional Fields (Form Logic):

- No conditional fields are explicitly defined in the code.

---

## 8. Dependencies:

### External Libraries:
1. **cxGrid**: For grid-based data display.
2. **SOAPHTTPClient**: For SOAP service communication.
3. **TClientDataSet**: For in-memory dataset management.

### Custom Components:
1. **TConsigneeServiceUtils**: Custom service for consignee data.
2. **TWarehouseNoDetailsServiceUtils**: Custom service for warehouse data.
3. **TShippingServiceUtils**: Custom service for shipping method data.

---

## 9. Fields and Validations Listing:

1. **stat**: (type: string, required, not explicitly validated in the code).
2. **whse**: (type: string, required, not explicitly validated in the code).
3. **whseName**: (type: string, optional, not explicitly validated in the code).
4. **shipMthd**: (type: string, required, not explicitly validated in the code).
5. **shipMthdName**: (type: string, optional, not explicitly validated in the code).
6. **payload**: (type: numeric, required, not explicitly validated in the code).
7. **stdCost**: (type: numeric, required, not explicitly validated in the code).

---

## 10. Examples and Diagrams:

### Flowchart:
Not applicable.

### Sequence Diagram:
Not applicable.

### Code Snippets:
```pascal
procedure TFRAMEconsStandardLogisticCosts.ACTaddExecute(Sender: TObject);
begin
  CDStable.Append;
end;
```

### Screenshots:
Not applicable.

---

## 11. Important Comments in the Code:

- `// SET DAS PROPRIEDADES DA FRAME`: Indicates where frame properties are set.
- `// configurar visibilidade de painel de ac��es e ac��es dispon�veis`: Configures the visibility of the action panel and available actions.

---

## 12. Conclusion:

The `FRconsStandardLogisticCosts` unit provides a robust framework for managing and displaying logistic cost data. Its strengths include a well-structured grid interface and integration with external services. However, the lack of explicit error handling and field validation could be improved.

---

## 13. Short Summary:

The `FRconsStandardLogisticCosts` unit manages logistic cost data using a grid interface, supporting actions like adding, deleting, and loading records. It integrates with SOAP services for data retrieval and updates, providing a comprehensive solution for logistics management.#### **FRconsStandardLogisticCosts.pas**

```
unit FRconsStandardLogisticCosts;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRGridEditSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, InvokeRegistry,
  cxEditRepositoryItems, ImgList, ActnList, ExtCtrls, Rio, SOAPHTTPClient,
  DBClient, StdCtrls, Buttons, cxGridLevel, cxGridCustomTableView,
  cxGridTableView, cxGridDBTableView, cxClasses, cxControls,
  cxGridCustomView, cxGrid, sFrameAdapter, kneFRGridManager, sBitBtn,
  sPanel,kneFGFindUtils;

type
  TFRAMEconsStandardLogisticCosts = class(TFRAMEBaseGridEditSOA)
    cxEDTshipMthd: TcxEditRepositoryButtonItem;
    PNL1: TsPanel;
    BTNloads: TsBitBtn;
    ACTloads: TAction;
    procedure cxDBVtableEditValueChanged(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure CDStableNewRecord(DataSet: TDataSet);
    procedure ACTaddExecute(Sender: TObject);
    procedure CDStableAfterScroll(DataSet: TDataSet);
    procedure ACTloadsExecute(Sender: TObject);
    procedure ACTdeleteExecute(Sender: TObject);
    procedure CDStableAfterPost(DataSet: TDataSet);
  private
    { Private declarations }
    procedure m_FindByCodeShipMethod(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure m_FindByCodeWhse(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure m_FindServiceShipMethod(Sender: TObject;
      AButtonIndex: Integer);
    procedure m_FindWhse(Sender: TObject; AButtonIndex: Integer);
    procedure m_SetAccessMode(Sender: TObject; var lv_mode: Boolean);
    procedure m_CalcLogCostsStat(const pv_ConsCd: string);

  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;

    procedure m_SetLoadsBtnStat;
  end;

var
  FRAMEconsStandardLogisticCosts: TFRAMEconsStandardLogisticCosts;

implementation

uses
  kneUtils, Global, kneTypes,
  kneFindDialog, kneDialogFactory,
  //---
  ConsigneeServiceUtils {#22894},
  WarehouseNoDetailsServiceUtils, ShippingServiceUtils;

{$R *.dfm}

{ TFRAMEconsStandardLogisticCosts   //JAR #16317 #16325  2013-04-23   }

constructor TFRAMEconsStandardLogisticCosts.Create(AOwner: TComponent);
begin
  inherited;
  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'consCode=cons';
  DataPacketName := 'ConsStdCost';
  PropertyName := 'cStdCost';
  FrameType := frtDetail;

  ProviderService := TConsigneeServiceUtils.Create(Self);// [02-11-2016, #22894]
  
  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := True;
  AvailableActions := 'ADD;DELETE';

  //Definir aqui os settings da dxGrid
  with GridSettings do
  begin
    // Campos Read-Only ........................................................
    // Campos Hidden ...........................................................
    DefineHiddenFields('HIDE_ALL_FIELDS');
    // Ordem Campos ............................................................
    DefineOrderFields('stat; whse; whseName; shipMthd; shipMthdName; payload; stdCost');
    // Key Fields ..............................................................
    KeyFields:= 'consCode;whse;shipMthd;payload';
    // Custom Editors ..........................................................
    AddCustomField('whse','cxEDTfind');
    AddCustomField('shipMthd','cxEDTshipMthd');
    AddCustomField('stat','cxEDTstat');
  end; //with
  ColsWidthInGrid := '100;60;220;80;160;100;100';

  // Atribui��o dos eventos dos Finds
  cxEDTfind.Properties.OnButtonClick := m_FindWhse;
  cxEDTshipMthd.Properties.OnButtonClick := m_FindServiceShipMethod;
```

#### **FRconsStandardLogisticCosts.dfm**

```
inherited FRAMEconsStandardLogisticCosts: TFRAMEconsStandardLogisticCosts
  inherited cxDBG: TcxGrid
    inherited cxDBVtable: TcxGridDBTableView
      OnEditValueChanged = cxDBVtableEditValueChanged
      OptionsData.Deleting = False
      OptionsData.Inserting = False
    end
  end
  inherited PNLfooter: TsPanel
    inherited PNLeditActions: TsPanel
      Width = 423
      object PNL1: TsPanel
        Left = 321
        Top = 1
        Width = 80
        Height = 30
        Align = alLeft
        TabOrder = 4
        SkinData.SkinSection = 'ALPHACOMBOBOX'
        object BTNloads: TsBitBtn
          Left = 3
          Top = 3
          Width = 70
          Height = 24
          Action = ACTloads
          Caption = 'Loads'
          TabOrder = 0
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000000000000000000000000000000000000FF00FF00FF00
            FF00D0D0D000CECECE00CECECE00CECECE00CECECE00CECECE00CECECE00CECE
            CE00CECECE00CECECE00CECECE00D0D0D000FF00FF00FF00FF00FF00FF009C9C
            9C00737373006F6F6F006F6F6F006F6F6F006F6F6F006F6F6F006F6F6F006F6F
            6F006F6F6F006F6F6F006F6F6F00737373009C9C9C00FF00FF00D28E4B00CB6E
            1800BE580000BA540000B6500000B24C0000AF490000AD470000AA440000A842
            0000A7410000A6410200AA470900A9601E0073737300D0D0D000E9BC9000DB9D
            6500D38E5500D48F5400D28D5200D28B5000D38B4D00D1884A00D0864600D185
            4200C0824400A4875500BF6A2900A94504006F6F6F00CECECE00EDC9A500D8A7
            8500F6E4D200EFCEAD00DEB39500F5E2CF00EDC9A500EACCB300F5E1CD00EBC3
            9C00A9997F000CCEFF00D0813B00A64000006F6F6F00CECECE00EECDAB00D59F
            7A00E6CBBB00E3BFA700D39F7D00E6CABA00E2BA9E00D5A38400E6CAB900E0B6
            9600CBA385004BDBFF00CF834100A74100006F6F6F00CECECE00EFD0B000D198
            7100E0B59600DCA77C00D39A7200DFB39300DBA47600D5A17D00DFB39200DAA1
            7000D4A4850090E9FF00CF854600A94300006F6F6F00CECECE00F0D3B500D9AB
            8D00FDF8F300F6E3D000D6A78B00FCF7F200F4E0CB00E2BFA900FCF6F000EED1
            B500CA906F00DEF8FF00D0874C00AB4500006F6F6F00CECECE00F1D5B800D59E
            7600C9885E00C9804B00C8855800C8855900C87E4800C8845700C8855800C87D
            4500A36B5E0052347600C98A5B00AD4700006F6F6F00CECECE00F2D6BA00E1BC
            A400FAEEE300F1D6BA00DFB79C00F9EDE200F0D1B100F6E4D100F9ECE000EECB
            A800B88A7F00365CFB009C6C6A00B14B00006F6F6F00CECECE00F2D7BC00DAAA
            8600EFCEAD00E8BD9600CF8C5800EECCAA00E7B99000CF8B5700EDC9A400E0AA
            7A00CE885200D5B39F00D29E7600B44E00006F6F6F00CECECE00F2D8BD00D98B
            3E00B7CA9300C3CD9B00CCCFA200CFD0A400CDCFA200C5CE9D00B9CB9400AAC7
            890099C27C0099A75300D88A3D00B85200006F6F6F00CECECE00F2D9BE00D785
            3400BFEFD000D2F4DD00DEF7E600E2F8EA00DFF7E700D4F4DF00C3F0D200AEEB
            C20095E5AF0091C07600D98C3F00BD5700006F6F6F00CECECE00F2D9BE00D786
            360065D4830080DB980086DB9A0087DB9A0084D995007CD68E0072D2850066CE
            7A0058CA6E005DA63A00D98D4100C25C000073737300D0D0D000F2D9BE00EFD0
            B000C3BA8000B8BD8000B6BA7B00B5B77500B3B46F00B1B06900B0AD6400AEA9
            5E00ADA65800C39A4D00E2A86E00CC6A0D009C9C9C00FF00FF00DB9B5B00EFCF
            AE00F2D9BE00F2D8BE00F2D8BD00F2D7BC00F2D6BA00F1D5B800F0D3B500F0D1
            B100EFCEAD00EECBA700EABE9300D28F4B00FF00FF00FF00FF00}
          SkinData.SkinSection = 'BUTTON'
          ImageIndex = 2
          Images = IMLeditActions
        end
      end
    end
  end
  inherited CDStable: TClientDataSet
    OnNewRecord = CDStableNewRecord
  end
  inherited ACLeditActions: TActionList
    object ACTloads: TAction
      Caption = 'Loads'
      ImageIndex = 6
      OnExecute = ACTloadsExecute
    end
  end
  inherited IMLeditActions: TImageList
    Bitmap = {
      494C010107000900040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000003000000001002000000000000030
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
```
<!-- tabs:end -->

