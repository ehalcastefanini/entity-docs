<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal:**  
  O código implementa um componente de interface gráfica chamado `TFRAMEsalesDirMkt`, que é uma extensão de um frame base (`TFRAMEBaseGridEditSOA`). Ele é utilizado para gerenciar e exibir dados relacionados a mercados de vendas (Sales Directory Market) em um formato de grade (grid). O objetivo principal é permitir a visualização, adição e exclusão de registros de mercado, além de configurar campos e realizar validações antes de salvar os dados.

* **Tecnologias Utilizadas:**  
  - **Delphi:** Linguagem de programação utilizada para criar a aplicação.
  - **Componentes cxGrid:** Para exibição de dados em formato de tabela.
  - **SOAP (Simple Object Access Protocol):** Para comunicação com serviços externos.
  - **DataSet e DBClient:** Para manipulação de dados e conexão com o banco de dados.

* **Forma do Componente:**  
  - **Exibição em Grade (Grid Display):**
    - **Colunas da Grade e seus Tipos:**
      - `marketCode` (Código do Mercado): Tipo string.
      - `marketDesc` (Descrição do Mercado): Tipo string.
    - **Ações da Grade e seus Efeitos:**
      - **Adicionar (`add`):** Permite adicionar um novo registro de mercado.
      - **Excluir (`delete`):** Permite excluir um registro existente.

---

## 2. Descrição da Funcionalidade:

* **Ações Disponíveis:**
  - Adicionar novos mercados.
  - Excluir mercados existentes.
  - Configurar campos da grade (visibilidade, ordem, editores personalizados).
  - Validar dados antes de salvar.

* **Componentes Principais:**
  - `cxGrid`: Componente de grade para exibição de dados.
  - `cxDBVtable`: Visualização de tabela vinculada ao banco de dados.
  - `cxEDTfindMarket`: Botão para buscar mercados.

* **Tradução para Pseudo-código:**
  - Evento `OnClick` do botão "Adicionar":  
    `if botão adicionar clicado then executar função ACTaddExecute`.
  - Evento `OnEditValueChanged` de um campo na grade:  
    `if valor do campo alterado then executar função cxDBVtableEditValueChanged`.
  - Evento `OnButtonClick` do botão de busca:  
    `if botão de busca clicado then executar função m_FindMarket`.

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  1. Inicialização do frame (`Create`):
     - Configurações iniciais, como campos chave, nome do pacote de dados e ações disponíveis.
     - Configuração da grade (campos ocultos, ordem, editores personalizados).
  2. Interações do Usuário:
     - Clique no botão "Adicionar" chama a função `ACTaddExecute`.
     - Alteração de valores na grade chama a função `cxDBVtableEditValueChanged`.
     - Clique no botão de busca chama a função `m_FindMarket`.

* **Dados Necessários:**
  - Código do mercado (`marketCode`).
  - Descrição do mercado (`marketDesc`).

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - **Adicionar:** Disponível sempre.
  - **Excluir:** Disponível sempre.
  - **Salvar:** Apenas se os dados forem válidos (verificação feita na função `VerifyDataBeforeSave`).

* **Filtros Disponíveis:**
  - Não há filtros explícitos definidos no código.

* **Mensagens de Erro:**
  - Não há mensagens de erro explícitas definidas no código.

* **Valores Padrão dos Campos:**
  - Não há valores padrão explícitos definidos no código.

* **Validações e Condições dos Campos:**
  - Validação de dados antes de salvar (`VerifyDataBeforeSave`).
  - Configuração de campos obrigatórios (`SetFieldRequiredState`).

---

## 5. Funções Principais:

* **`Create`:**  
  Configura o frame, define campos chave, ações disponíveis e configurações da grade.

* **`ACTaddExecute`:**  
  Executa a ação de adicionar um novo registro.

* **`cxDBVtableEditValueChanged`:**  
  Manipula alterações nos valores dos campos da grade.

* **`VerifyDataBeforeSave`:**  
  Verifica se os dados são válidos antes de salvar.

* **`m_FindMarket`:**  
  Realiza a busca de mercados.

---

## 6. Consumo de Serviços API:

* **Nenhuma chamada explícita a serviços externos foi identificada no código fornecido.**

---

## 7. Campos Condicionais (Lógica do Formulário):

* Não há campos condicionais explícitos definidos no código.

---

## 8. Dependências:

* **Bibliotecas Externas:**
  - `cxGrid`, `cxDBData`: Para exibição de dados em grade.
  - `SOAPHTTPClient`: Para comunicação com serviços SOAP.
  - `DBClient`: Para manipulação de dados.

* **Componentes Customizados:**
  - `TFRAMEBaseGridEditSOA`: Frame base herdado.
  - `TkneFGControlsUtils`: Utilitário para manipulação de controles.

---

## 9. Listagem de Campos e Validações:

* **Campos:**
  - `marketCode` (tipo: string, obrigatório, não definido no código).
  - `marketDesc` (tipo: string, obrigatório, não definido no código).

* **Mapeamento de Valores e Colunas do Banco de Dados:**
  - `marketCode` → Coluna correspondente no banco.
  - `marketDesc` → Coluna correspondente no banco.

---

## 10. Exemplos e Diagramas:

* **Diagrama de Fluxo:**  
  Não aplicável.

* **Diagrama de Sequência:**  
  Não aplicável.

* **Exemplo de Código:**  
  ```delphi
  procedure TFRAMEsalesDirMkt.ACTaddExecute(Sender: TObject);
  begin
    // Lógica para adicionar um novo mercado
  end;
  ```

* **HTML Representando a Grade:**  
  ```html
  <table style="width: 100%; border: 1px solid black;">
    <thead>
      <tr>
        <th style="width: 100px;">Código do Mercado</th>
        <th style="width: 150px;">Descrição do Mercado</th>
      </tr>
    </thead>
    <tbody>
      <tr>
        <td>MKT001</td>
        <td>Mercado A</td>
      </tr>
      <tr>
        <td>MKT002</td>
        <td>Mercado B</td>
      </tr>
    </tbody>
  </table>
  ```

---

## 11. Comentários Importantes no Código:

* **Configuração Inicial do Frame:**
  ```delphi
  MasterKeyFields := 'salesdir';
  DataPacketName := 'SalesdirMkt';
  PropertyName := 'markets';
  FrameType := frtDetail;
  ```

* **Configuração da Grade:**
  ```delphi
  HiddenFields.Add('HIDE_ALL_FIELDS');
  OrderFields.Add('marketCode');
  OrderFields.Add('marketDesc');
  ```

---

## 12. Conclusão:

O código implementa um frame para gerenciar mercados de vendas, com funcionalidades de exibição, adição e exclusão de registros. Ele é bem estruturado, mas carece de mensagens de erro e validações explícitas. Sua integração com serviços externos não está clara no código fornecido.

---

## 13. Resumo Curto:

O `TFRAMEsalesDirMkt` é um frame para gerenciar mercados de vendas, permitindo exibição, adição e exclusão de registros em uma grade configurável. Ele utiliza componentes visuais e validações básicas para garantir a integridade dos dados.#### **FRsalesDirMkt.pas**

```
unit FRsalesDirMkt;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRGridEditSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, InvokeRegistry,
  cxEditRepositoryItems, sFrameAdapter, ImgList, ActnList, ExtCtrls, Rio,
  SOAPHTTPClient, DBClient, kneFRGridManager, StdCtrls, Buttons, sBitBtn,
  sPanel, cxGridLevel, cxClasses, cxControls, cxGridCustomView,
  cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGrid,
  kneFREditSOA;

type
  TFRAMEsalesDirMkt = class(TFRAMEBaseGridEditSOA)
    cxEDTfindMarket: TcxEditRepositoryButtonItem;
    procedure ACTaddExecute(Sender: TObject);
    procedure cxDBVtableEditValueChanged(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);

  private
    { Private declarations }
    procedure m_OnSetAccessMode(Sender: TObject; var pv_value: Boolean);
    function VerifyDataBeforeSave: Boolean;
    procedure m_ConfigFields(const Dataset: TDataSet;
      const Datapacket: string);
    function SetFieldRequiredState(const pv_Field: TField;
      pv_State: Boolean): Boolean;
    procedure m_FindMarket(Sender: TObject; AButtonIndex: Integer);
    procedure m_FindByCodeMarket(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  end;

var
  FRAMEsalesDirMkt: TFRAMEsalesDirMkt;

const
  gc_ColsWith =  '100;150;';

implementation

uses
  kneTypes, kneFGControlsUtils, Global, kneUtils, kneFindDialog,
  kneDialogFactory, kneFGFindUtils,
  //Frames, Forms
  MsalesDir,
  // ServiceUtils
  CustomerMarketServiceUtils;

{$R *.dfm}

{ TFRAMEsalesDirMkt }

constructor TFRAMEsalesDirMkt.Create(AOwner: TComponent);
var
  lv_form:  TCustomForm;
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'salesdir';
  DataPacketName := 'SalesdirMkt';        // o nome do detail no datapacket(metadata) � sempre no singular
  PropertyName := 'markets';                  // nome do campo da metadata que vai conter os details
  FrameType := frtDetail;

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  AvailableActions := 'add;delete';

  //Definir aqui os settings da dxGrid
  with GridSettings do
  begin
    // Campos Read-Only ........................................................
    // Campos Hidden ...........................................................
    HiddenFields.Add('HIDE_ALL_FIELDS');
    // Ordem Campos ............................................................
    OrderFields.Add('marketCode');
    OrderFields.Add('marketDesc');
    // Key Fields ..............................................................
    KeyFields:= 'salesdir';
    // Custom Editors ..........................................................
    AddCustomField('marketCode','cxEDTfindMarket');
    UseColsBestFit := False;
  end; //with

  OnSetAccessMode := m_OnSetAccessMode;
  cxEDTfindMarket.Properties.OnButtonClick := m_FindMarket;

  lv_form := TkneFGControlsUtils.fg_GetOwnerForm(Self);
end;

procedure TFRAMEsalesDirMkt.m_OnSetAccessMode(Sender: TObject;var pv_value: Boolean);
begin
  inherited;

```

#### **FRsalesDirMkt.dfm**

```
inherited FRAMEsalesDirMkt: TFRAMEsalesDirMkt
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

