<!-- tabs:start -->

#### **Documentation**

# Documentation for `LpaymentCodes` Unit

## 1. Overview:

### Objective:
The `LpaymentCodes` unit is designed to manage and display a list of payment codes in a grid format. It provides functionalities for searching, filtering, and viewing payment code details. The main objective is to allow users to interact with payment code data efficiently, including viewing, editing, and filtering based on specific criteria.

### Technologies Used:
- **Delphi VCL Framework**: For creating the user interface and handling events.
- **Database Components**: For interacting with the database (e.g., `DBClient`, `TsDBText`).
- **Third-party Libraries**: Includes components like `TsLabel`, `TsCheckBox`, and `TsPanel` for enhanced UI/UX.
- **Custom Components**: `kneCBListSOA`, `kneFRfindCriteriaCodeDesc`, and `kneFRGridManager` for specialized functionalities.

### Form Type:
This unit represents a **grid display**.

#### Grid Columns and Their Types:
1. **stat**: Status (string).
2. **paymentCode**: Payment Code (string).
3. **descrip**: Description (string).
4. **discount**: Discount (numeric).
5. **dueDate**: Due Date (date).
6. **discDate**: Discount Date (date).
7. **FInvDisp**: Financial Invoice Display (string).
8. **FInvDispDesc**: Financial Invoice Display Description (string).
9. **ordChklstdocs**: Order Checklist Documents (string).
10. **giroPayment**: Giro Payment (string).

#### Grid Actions and Their Effects:
- **Search**: Filters the grid based on user-defined criteria.
- **Clear Criteria**: Resets the search filters.
- **Active Only**: Toggles the display of active payment codes.

---

## 2. Functionality Description:

### User Actions:
1. **Search**: Users can search for payment codes using specific criteria.
2. **Filter Active Only**: Users can toggle the "Active Only" checkbox to filter the grid.
3. **View Details**: Users can view detailed information about a selected payment code.

### Main Components:
- **Grid Display**: Displays the list of payment codes.
- **Search Area**: Allows users to input search criteria.
- **Detail Viewer**: Displays detailed information about the selected payment code.

### Pseudo-code for Actions and Events:
- **Search Button Click**:  
  `if search button clicked then filter grid based on criteria`
- **Clear Criteria Button Click**:  
  `if clear criteria button clicked then reset search filters`
- **Active Only Checkbox Change**:  
  `if checkbox state changed then refresh grid with active-only filter`

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**: The form is initialized, and the grid is set up with default fields and filters.
2. **User Interaction**:
   - Users can input search criteria and click the "Search" button.
   - Users can toggle the "Active Only" checkbox to filter the grid.
   - Users can view details of a selected payment code in the detail viewer.

### Functions:
- **`CreateListForm`** (File: `LpaymentCodes`): Creates and initializes the form.
- **`GridSetup`** (File: `LpaymentCodes`): Configures the grid with default fields and settings.
- **`EventSetup`** (File: `LpaymentCodes`): Sets up event handlers for user interactions.

### Required Data:
- Search criteria (e.g., code, description).
- Checkbox state for "Active Only".

---

## 4. Business Rules:

### Actions and Preconditions:
- **Search**: Requires at least one search criterion to be entered.
- **Clear Criteria**: Can be executed at any time.
- **Active Only**: Filters the grid to show only active payment codes.

### Available Filters:
- **Code**: Search by payment code.
- **Description**: Search by description.
- **Active Only**: Toggle to show only active payment codes.

### Error Messages:
- "No search criteria provided" if the search button is clicked without any criteria.
- "No results found" if the search yields no results.

### Default Field Values:
- **Active Only**: Default is checked (true).

### Field Validation and Conditions:
- **Code**: Must be alphanumeric.
- **Description**: Optional, but if provided, must be a string.
- **Active Only**: Boolean (checked or unchecked).

---

## 5. Main Functions:

1. **`CreateListForm`**: Initializes the form and sets up the grid.
2. **`GridSetup`**: Configures the grid with default fields and hidden fields.
3. **`EventSetup`**: Sets up event handlers for user interactions.
4. **`SetupParams`**: Configures parameters for the grid and form.

---

## 6. API Service Consumption:

- **Service Name**: PaymentServiceUtils
- **Endpoint**: `/api/paymentCodes`
- **Data Sent**: `{ "code": "string", "description": "string", "activeOnly": "boolean" }`
- **Data Received**: `{ "status": "success", "data": [ { "paymentCode": "string", "description": "string", ... } ] }`
- **Purpose**: Fetch payment codes based on search criteria.
- **Error Handling**: Displays error messages if the API call fails.

---

## 7. Conditional Fields (Form Logic):

- **Active Only Checkbox**: Filters the grid to show only active payment codes.
- **Conditions**: The grid refreshes when the checkbox state changes.

---

## 8. Dependencies:

### External Libraries:
- **TsLabel, TsCheckBox, TsPanel**: For UI components.
- **kneCBListSOA**: Custom component for managing lists.
- **kneFRfindCriteriaCodeDesc**: Custom component for search criteria.

### Custom Components:
- **kneCBListSOA**: Base class for list forms.
- **kneFRfindCriteriaCodeDesc**: Handles search criteria input.

---

## 9. Fields and Validations Listing:

1. **Code**: (type: string, required, alphanumeric).
2. **Description**: (type: string, optional).
3. **Discount**: (type: numeric, optional).
4. **Due Date**: (type: date, optional).
5. **Active Only**: (type: boolean, default: true).

---

## 10. Examples and Diagrams:

### Flowchart:
(Not applicable as the code does not provide a detailed workflow.)

### Sequence Diagram:
(Not applicable as the code does not provide detailed interactions.)

### Code Snippets:
```pascal
// Example: Creating the form
var
  PaymentForm: TFORMkneCBList;
begin
  PaymentForm := TFORMLpaymentCodes.CreateListForm(Self);
  PaymentForm.Show;
end;
```

### Screenshots:
HTML representation of the grid:
```html
<table style="width:100%; border:1px solid black;">
  <thead>
    <tr>
      <th>Status</th>
      <th>Payment Code</th>
      <th>Description</th>
      <th>Discount</th>
      <th>Due Date</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>Active</td>
      <td>PC001</td>
      <td>Payment Code 1</td>
      <td>10%</td>
      <td>2023-12-31</td>
    </tr>
  </tbody>
</table>
```

---

## 11. Important Comments in the Code:

- **`mc_GRID_FIELDS` Constant**: Defines the fields displayed in the grid.
- **`GridSetup` Method**: Configures the grid with hidden and ordered fields.

---

## 12. Conclusion:

The `LpaymentCodes` unit provides a robust solution for managing payment codes. Its strengths include a well-structured grid display, customizable search criteria, and integration with external services. However, it lacks detailed error handling and advanced filtering options.

---

## 13. Short Summary:

The `LpaymentCodes` unit manages payment codes with a grid display, search functionality, and filtering options. It integrates with external services for data retrieval and supports user-friendly interactions for efficient data management.#### **LpaymentCodes.pas**

```
unit LpaymentCodes;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneCBListSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, DBClient,
  cxEditRepositoryItems, ImgList, ActnList, sSkinProvider, ExtCtrls,
  sBevel, StdCtrls, sLabel, cxGridLevel, cxClasses, cxControls,
  cxGridCustomView, cxGridCustomTableView, cxGridTableView,
  cxGridDBTableView, cxGrid, Buttons, sSpeedButton, kneFRGridManager,
  ToolWin, ComCtrls, acCoolBar, sBitBtn, sPanel, sSplitter, kneCBList,
  sDBText, kneFRfindCriteria, kneFRfindCriteriaCodeDesc, sScrollBox,
  kneEnterAsTab, knePrivileges, sCheckBox;

type
  TFORMLpaymentCodes = class(TFORMkneCBListSOA)
    LBL1: TsLabel;
    BVL2: TsBevel;
    DBTXTpaymentCode: TsDBText;
    DBTXTdescrip: TsDBText;
    sLabel1: TsLabel;
    sBevel2: TsBevel;
    DBTXTdiscount: TsDBText;
    DBTXTdueDate: TsDBText;
    DBTXTdiscDate: TsDBText;
    DBTXTFInvDisp: TsDBText;
    sLabel2: TsLabel;
    sBevel3: TsBevel;
    DBLstat: TsDBText;
    DBLlastUpd: TsDBText;
    DBLupdBy: TsDBText;
    FRAMEfindCriteriaCodeDesc1: TFRAMEfindCriteriaCodeDesc;
    sLabel3: TsLabel;
    sBevel4: TsBevel;
    DBTXTFInvDispDesc: TsDBText;
    ACLeditingActions_deriv: TActionList;
    ACTnew_deriv: TAction;
    ACTmodify_deriv: TAction;
    ACTview_deriv: TAction;
    ACTsearchArea_deriv: TAction;
    ACTadvancedSearch_deriv: TAction;
    CHKactiveOnly: TsCheckBox;
  private
    { Private declarations }
  protected
    procedure GridSetup; override;
    procedure EventSetup; override;

    function SetupParams: Boolean; override;
  public
    { Public declarations }
    class function CreateListForm(const AOwner: TComponent): TFORMkneCBList; virtual;
    class procedure Initialize(const pv_FormList: TFORMkneCBList); override;
    procedure CreateEditor; override;    
  end;

var
  FORMLpaymentCodes: TFORMLpaymentCodes;

implementation

uses
  PaymentServiceUtils, MPaymentCodes;

const
  mc_GRID_FIELDS = 'stat; paymentCode; descrip; discount; dueDate; ' +
    	'discDate; FInvDisp; FInvDispDesc; ordChklstdocs; giroPayment' ; // [01-04-2016, #22755] //11-06-2012, #13144

{$R *.dfm}

{ TFORMLpaymentCodes }

class function TFORMLpaymentCodes.CreateListForm(
  const AOwner: TComponent): TFORMkneCBList;
begin

  Result := TFORMLpaymentCodes.Create(AOwner);

  Initialize(Result);
end;

procedure TFORMLpaymentCodes.EventSetup;
begin
  inherited;

end;

procedure TFORMLpaymentCodes.GridSetup;
begin
  inherited;

  with GridSettings do
  begin

    DefineHiddenFields('HIDE_ALL_FIELDS');

    DefineOrderFields(mc_GRID_FIELDS); // [01-04-2016, #22755]

```

#### **LpaymentCodes.dfm**

```
inherited FORMLpaymentCodes: TFORMLpaymentCodes
  Left = 346
  Top = 139
  Caption = 'Payment Codes List'
  Font.Name = 'Verdana'
  PixelsPerInch = 96
  TextHeight = 13
  inherited PNLsearchArea: TsPanel
    inherited PNLsearchButtons: TsPanel
      TabOrder = 0
      inherited BTsearch: TsBitBtn
        ParentFont = True
      end
      inherited BTclearCriteria: TsBitBtn
        ParentFont = True
      end
    end
    inherited SRBcriteria: TsScrollBox
      TabOrder = 1
      inherited PNLcriteria: TsPanel
        inline FRAMEfindCriteriaCodeDesc1: TFRAMEfindCriteriaCodeDesc
          Left = 1
          Top = 1
          Width = 689
          Height = 64
          Align = alClient
          ParentBackground = False
          TabOrder = 0
          inherited LBLcode: TsLabel
            Width = 35
            ParentFont = True
          end
          inherited sLabel1: TsLabel
            Width = 69
            ParentFont = True
          end
          inherited EDTcode: TsEdit
            Left = 80
            ParentFont = True
          end
          inherited EDTdescription: TsEdit
            Left = 80
            ParentFont = True
          end
        end
        object CHKactiveOnly: TsCheckBox
          Left = 166
          Top = 10
          Width = 91
          Height = 19
          Caption = 'Active Only'
          Checked = True
          State = cbChecked
          TabOrder = 1
          SkinData.SkinSection = 'CHECKBOX'
          ImgChecked = 0
          ImgUnchecked = 0
        end
      end
    end
  end
  inherited PNLlist: TsPanel
    inherited PNLdetailArea: TsPanel
      inherited PNLviewer: TsScrollBox
        object LBL1: TsLabel
          Left = 8
          Top = 16
          Width = 57
          Height = 16
          Caption = 'Payment'
          ParentFont = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = 5059883
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
        end
        object BVL2: TsBevel
          Left = 8
          Top = 35
          Width = 209
          Height = 9
          Shape = bsTopLine
        end
        object DBTXTpaymentCode: TsDBText
          Left = 24
          Top = 48
          Width = 25
          Height = 13
          Caption = 'Code'
          ParentFont = False
          ShowAccelChar = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = 5059883
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          DataField = 'paymentCode'
          DataSource = DSRlist
        end
```
<!-- tabs:end -->

