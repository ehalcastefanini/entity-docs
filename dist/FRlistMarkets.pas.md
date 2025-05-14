<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRlistMarkets` Code Unit

---

## 1. Overview:

### Objective and Problem Solved:
The `FRlistMarkets` code unit defines a Delphi frame (`TFRAMElistMarkets`) that provides a grid-based interface for managing and editing market data associated with agents. It allows users to perform actions such as adding, deleting, and searching for markets. The main objective is to provide a user-friendly interface for managing market data efficiently.

### Technologies Used:
- **Delphi VCL Framework**: For building the user interface and handling events.
- **SOAP Services**: For interacting with external services to fetch and update market data.
- **Database Components**: For managing and displaying data from a database.
- **cxGrid**: A component from DevExpress for displaying and managing tabular data.

### Form Type:
This is a **grid display** form.

#### Grid Columns and Their Types:
1. **Market**: String (Editable).
2. **Market Name**: String (Read-only).
3. **Agent**: String (Hidden).

#### Grid Actions and Their Effects:
1. **Add**: Adds a new market entry.
2. **Delete**: Deletes the selected market entry.
3. **Search**: Allows users to search for a market using a dialog.

---

## 2. Functionality Description:

### User/Software Actions:
1. Add a new market.
2. Delete an existing market.
3. Search for a market using a dialog.
4. Edit market details in the grid.

### Main Components:
- **Grid (`cxGrid`)**: Displays market data.
- **Search Button (`cxEDTfindMarket`)**: Triggers the search dialog.
- **Action List**: Handles add and delete actions.

### Pseudo-code for Actions and Events:
- `OnClick` event of Add button: `if add button clicked then execute ACTaddExecute`.
- `OnClick` event of Delete button: `if delete button clicked then execute ACTdeleteExecute`.
- `OnEditValueChanged` event of grid: `if grid cell value changed then execute cxDBVtableEditValueChanged`.
- `OnButtonClick` event of search button: `if search button clicked then execute m_FindMarket`.

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The frame is initialized with specific settings for the grid, such as read-only fields, hidden fields, and key fields.
   - The search button is configured with an event handler (`m_FindMarket`).

2. **User Interactions**:
   - Clicking the "Add" button triggers the `ACTaddExecute` method to add a new market.
   - Clicking the "Delete" button triggers the `ACTdeleteExecute` method to delete the selected market.
   - Clicking the search button opens a dialog to search for a market.

### Data Requirements:
- Users must provide market details (e.g., market code, market name) when adding or editing entries.

---

## 4. Business Rules:

### Actions and Preconditions:
1. **Add**:
   - Preconditions: None.
   - Action: Adds a new market entry to the grid.

2. **Delete**:
   - Preconditions: A market must be selected in the grid.
   - Action: Deletes the selected market entry.

3. **Search**:
   - Preconditions: None.
   - Action: Opens a dialog to search for a market.

### Available Filters:
- No explicit filters are defined in the code.

### Error Messages:
- No error messages are explicitly defined in the code.

### Default Field Values:
- No default values are explicitly defined in the code.

### Field Validation and Conditions:
- **Market Name**: Read-only.
- **Agent**: Hidden.
- **Market**: Editable.

---

## 5. Main Functions:

1. **`ACTaddExecute`**:
   - Adds a new market entry.
2. **`ACTdeleteExecute`**:
   - Deletes the selected market entry.
3. **`m_FindMarket`**:
   - Opens a search dialog to find a market.
4. **`cxDBVtableEditValueChanged`**:
   - Handles changes to grid cell values.

---

## 6. API Service Consumption:

### External Service Calls:
1. **Service Name**: CustomerMarketServiceUtils.
   - **Purpose**: Interacts with the backend to fetch and update market data.
   - **Details**: Not explicitly defined in the code.

---

## 7. Conditional Fields (Form Logic):

- **Search Button (`cxEDTfindMarket`)**:
  - Visible and functional at all times.

---

## 8. Dependencies:

### External Libraries:
1. **DevExpress Components**: Used for grid and UI elements.
2. **SOAPHTTPClient**: For SOAP-based service communication.

### Custom Components:
1. **TFRAMEBaseGridEditSOA**: Base class for the frame.
2. **TkneDialogFactory**: Factory for creating search dialogs.

---

## 9. Fields and Validations Listing:

1. **Market**:
   - Type: String.
   - Editable: Yes.
   - Validation: Not explicitly defined in the code.
2. **Market Name**:
   - Type: String.
   - Editable: No (Read-only).
3. **Agent**:
   - Type: String.
   - Editable: No (Hidden).

---

## 10. Examples and Diagrams:

### Flowchart:
```plaintext
[Start] --> [Initialize Frame] --> [Load Grid Data] --> [User Interaction]
    --> [Add Market] --> [Update Grid]
    --> [Delete Market] --> [Update Grid]
    --> [Search Market] --> [Open Search Dialog]
```

### Sequence Diagram:
```plaintext
User --> Frame: Click Add/Delete/Search
Frame --> Backend: Fetch/Update Data
Backend --> Frame: Return Data
Frame --> User: Update Grid
```

### Code Snippets:
```delphi
procedure TFRAMElistMarkets.ACTaddExecute(Sender: TObject);
begin
  // Code to add a new market
end;
```

### Screenshots:
Not applicable (no DFM file provided).

---

## 11. Important Comments in the Code:

1. **Grid Settings**:
   - Read-only fields: `marketName`.
   - Hidden fields: `agent`.
   - Key fields: `agent;market`.

2. **Search Button**:
   - Configured with `m_FindMarket` event handler.

---

## 12. Conclusion:

The `FRlistMarkets` code unit provides a robust and user-friendly interface for managing market data. Its strengths include a well-structured grid and integration with search functionality. However, the lack of explicit error handling and field validations may limit its robustness in certain scenarios.

---

## 13. Short Summary:

The `FRlistMarkets` unit implements a grid-based interface for managing market data, supporting actions like add, delete, and search. It integrates with SOAP services and uses DevExpress components for UI rendering.#### **FRlistMarkets.pas**

```
unit FRlistMarkets;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRGridEditSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, InvokeRegistry,
  cxEditRepositoryItems, sFrameAdapter, ImgList, ActnList, ExtCtrls, Rio,
  SOAPHTTPClient, DBClient, kneFRGridManager, StdCtrls, Buttons, sBitBtn,
  sPanel, cxGridLevel, cxClasses, cxControls, cxGridCustomView,
  cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGrid;

type
  TFRAMElistMarkets = class(TFRAMEBaseGridEditSOA)
    cxEDTfindMarket: TcxEditRepositoryButtonItem;
    procedure ACTaddExecute(Sender: TObject);
    procedure cxDBVtableEditValueChanged(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure ACTdeleteExecute(Sender: TObject);
  private
    { Private declarations }
    procedure m_FindByCodeMarket(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure m_FindMarket(Sender: TObject; AButtonIndex: Integer);
    procedure UpDateAgentMarkets;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  end;

var
  FRAMElistMarkets: TFRAMElistMarkets;

//const
//  gc_ColsWith =  '100;150;';

implementation

uses
  kneTypes, kneUtils, kneFindDialog, kneDialogFactory, Global, kneFGFindUtils,
  kneFREditSOA, kneConfigObjects,
  //Forms
  Magents,
  // ServiceUtils
  CustomerMarketServiceUtils;

{$R *.dfm}

constructor TFRAMElistMarkets.Create(AOwner: TComponent);
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'Code=agent';
  DataPacketName := 'AgentMarket';
  PropertyName := 'markets';
  FrameType := frtDetail;

  AvailableActions := 'ADD;DELETE';

  ServiceParams.ShowInactives := True;

  //Definir aqui os settings da dxGrid
  with GridSettings do
  begin
    // Campos Read-Only ........................................................ 
    DefineReadOnlyFields('marketName;');
    // Campos Hidden ...........................................................
    DefineHiddenFields('agent;');
    // Ordem Campos ............................................................
    DefineOrderFields('market; marketName;');
    // Key Fields ..............................................................
    KeyFields:= 'agent;market';
    // Custom Editors ..........................................................
    AddCustomField('market','cxEDTfindMarket');
  end; //with

  cxEDTfindMarket.Properties.OnButtonClick := m_FindMarket;

end;

//---------------- Finds
procedure TFRAMElistMarkets.m_FindMarket(Sender: TObject;
  AButtonIndex: Integer);
var
  lv_Find: TFORMkneFindDialog;
  lv_AgentMarketsList: string;
begin

  try    // inicializa��o de Find Dialog
    lv_Find := nil;
    lv_Find := TkneDialogFactory.GetFindDialog(Application);

    // campos para selec��o do Find DataSet
    lv_Find.Options.DataSelection.FieldNameForCode:= 'marketCode';                        //TODO: JAR 19-07-2007 Verificar o nome dos campos (m_FindProductGrade)
    lv_Find.Options.DataSelection.FieldNamesForDesc.Clear;
    lv_Find.Options.DataSelection.FieldNamesForDesc.Add('description');

    lv_Find.Options.DataSelection.TargetDataSet:= CDStable;
```

#### **FRlistMarkets.dfm**

```
inherited FRAMElistMarkets: TFRAMElistMarkets
  inherited cxDBG: TcxGrid
    inherited cxDBVtable: TcxGridDBTableView
      OnEditValueChanged = cxDBVtableEditValueChanged
    end
  end
  inherited cxEDTR: TcxEditRepository
    object cxEDTfindMarket: TcxEditRepositoryButtonItem
      Properties.Buttons = <
        item
          Default = True
          Glyph.Data = {
            36030000424D3603000000000000360000002800000010000000100000000100
            18000000000000030000120B0000120B00000000000000000000FF00FF4A667C
            BE9596FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FF6B9CC31E89E84B7AA3C89693FF00FFFF00FFFF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF4BB4FE51B5FF
            2089E94B7AA2C69592FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FF51B7FE51B3FF1D87E64E7AA0CA9792FF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            51B7FE4EB2FF1F89E64E7BA2B99497FF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF52B8FE4BB1FF2787D95F6A76FF
            00FFB0857FC09F94C09F96BC988EFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            FF00FFFF00FF55BDFFB5D6EDBF9D92BB9B8CE7DAC2FFFFE3FFFFE5FDFADAD8C3
            B3B58D85FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFCEA795FD
            EEBEFFFFD8FFFFDAFFFFDBFFFFE6FFFFFBEADDDCAE837FFF00FFFF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFC1A091FBDCA8FEF7D0FFFFDBFFFFE3FFFFF8FFFF
            FDFFFFFDC6A99CFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFC1A091FEE3ACF1
            C491FCF2CAFFFFDDFFFFE4FFFFF7FFFFF7FFFFE9EEE5CBB9948CFF00FFFF00FF
            FF00FFFF00FFFF00FFC2A191FFE6AEEEB581F7DCAEFEFDD8FFFFDFFFFFE3FFFF
            E4FFFFE0F3ECD2BB968EFF00FFFF00FFFF00FFFF00FFFF00FFBC978CFBE7B7F4
            C791F2C994F8E5B9FEFCD8FFFFDDFFFFDCFFFFE0E2D2BAB68E86FF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFD9C3A9FFFEE5F7DCB8F2C994F5D4A5FAE8BDFDF4
            C9FDFBD6B69089FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFB58D85E8
            DEDDFFFEF2F9D8A3F4C48CF9D49FFDEAB8D0B49FB89086FF00FFFF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFFF00FFAD827FC9AA9EEFE0B7EFDFB2E7CEACB890
            86B89086FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
            00FFFF00FFBA968ABB988CB79188FF00FFFF00FFFF00FFFF00FF}
          Kind = bkGlyph
        end>
      Properties.CharCase = ecUpperCase
      Properties.ClickKey = 114
    end
  end
end
```
<!-- tabs:end -->

